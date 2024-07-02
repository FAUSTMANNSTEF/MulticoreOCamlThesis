open Kcas

(** Atomic Markable References using the kcas library for atomic multi-word CAS *)
module type ATOMIC_MARKABLE = sig
  type 'a markable
  val markable_CAS : 'a markable -> 'a -> bool -> 'a -> bool -> bool
  val get_reference : 'a markable -> 'a
  val get_mark : 'a markable -> bool
  val get_marked : 'a markable -> 'a * bool 
  val make_markable : 'a -> bool -> 'a markable
end

module AtomicMarkable : ATOMIC_MARKABLE = struct
  type 'a markable = {
    ref : 'a Kcas.Loc.t;
    mark : bool Kcas.Loc.t
  }
  let markable_CAS r exRef exMark newRef newMark =
    let tx ~xt =
      let _ = Xt.compare_and_set ~xt r.ref exRef newRef in
      Xt.compare_and_set ~xt r.mark exMark newMark
    in Xt.commit { tx }
  let get_mark r = Loc.get r.mark
  let get_reference r = Loc.get r.ref
  let get_marked r =
    (Loc.get r.ref, Loc.get r.mark)
  let make_markable value mark =
    { ref = Loc.make value; mark = Loc.make mark}
end

(** Usage example *)

open AtomicMarkable

(* Node structure in linked list *)
type 'a node = {
  value : 'a;
  key : int;
  next : ('a node) markable option
}

(* Barrier used to force threads to start working at the same time *)
type barrier = {
  waiters : int Atomic.t;
  size : int;
  passed : int Atomic.t
}

let make_node value next = 
  let key = Hashtbl.hash value in
  { value = value; key = key; next = next }

type 'a linkedlist = {
  mutable firstnode: 'a node;
  mutable lastnode: 'a node;
}

let create_linkedlist () : 'a linkedlist =
  let sentinel2 = {
    value = max_int;
    key = max_int;
    next = None
  } in
  {
    firstnode = {
      value = min_int;
      key = min_int;
      next = Some (make_markable sentinel2 false)
    };
    lastnode = sentinel2
  }


(* Creates a new barrier *)
let create_barrier n = {
  waiters = Atomic.make n;
  size = n;
  passed = Atomic.make 0
}

(* Barrier synchronization function *)
let await { waiters; size; passed } =
  if Atomic.fetch_and_add passed 1 = size - 1 then (
    Atomic.set passed 0;
    Atomic.set waiters 0
  );

  while Atomic.get waiters = size do
    Domain.cpu_relax ()
  done;

  Atomic.incr waiters;
  while Atomic.get waiters < size do
    Domain.cpu_relax ()
  done

let print_list linkedlist =
  let rec print_node n = 
    Printf.printf "%d (%d) : " n.value n.key;
    match n.next with
    | None -> ()
    | Some nxt ->
      print_node (get_reference nxt)
  in
  print_node (linkedlist.firstnode);
  print_newline ()

type 'a window = {
  pred: 'a node;
  curr: 'a node;
}

let find_window (linkedlist : 'a linkedlist) key : 'a window =
  let  retry () =
    let pred = ref linkedlist.firstnode in
    let curr = ref (match !pred.next with 
      | Some nxt -> get_reference nxt 
      | None -> raise (Invalid_argument "find_window: next node is None")) in
    let marked = ref false in
    let continue_traversal = ref true in
    while !continue_traversal do
      try
        while true do
          match !curr.next with
          | None -> raise Exit
          | Some nxt -> 
            let current_value, current_mark = get_marked nxt in
            marked := current_mark;
            let succ = ref current_value in
            while !marked do
              let snip = markable_CAS (match !pred.next with Some p -> p | None -> raise Exit) !curr false !succ false in
              if not snip then raise Exit;
              curr := !succ;
              let current_value, current_mark = get_marked (match !curr.next with Some c -> c | None -> raise Exit) in
              marked := current_mark;
              succ := current_value;
            done;
            if !curr.key >= key then (
              continue_traversal := false;
              raise Exit
            );
            pred := !curr;
            curr := !succ;
        done
      with Exit ->
        continue_traversal := false
    done;
    let p = !pred in
    let c = !curr in
    { pred = p ; curr = c }
  in
  retry ()

let additem linkedlist value =
  let key = Hashtbl.hash value in
  let rec loop () =
    let find = find_window linkedlist key in
    let pred = find.pred in
    let curr = find.curr in
    if curr.key = key then
      false
    else
      let node = make_node value (Some (make_markable curr false)) in
      if markable_CAS (match pred.next with Some p -> p | None -> raise Exit) curr false node false then begin
        true
      end else 
        loop ()
  in
  loop ()

let removeitem linkedlist value =
  let key = Hashtbl.hash value in
  let rec loop () = 
    let find = find_window linkedlist key in
    let pred = find.pred in
    let curr = find.curr in
    if curr.key <> key then 
      false
    else 
      let succ = get_reference (match curr.next with Some c -> c | None -> raise Exit) in
      let snip = markable_CAS (match curr.next with Some c -> c | None -> raise Exit) succ false succ true in
      if not snip then loop ()
      else 
        let _ = markable_CAS (match pred.next with Some p -> p | None -> raise Exit) curr false succ false in
        true
  in
  loop ()

let contains linkedlist value =
  let marked = ref false in
  let key = Hashtbl.hash value in
  let curr = ref (get_reference (match linkedlist.firstnode.next with Some f -> f | None -> raise Exit)) in
  while !curr.key < key do
    let current_value, current_mark = get_marked (match !curr.next with Some c -> c | None -> raise Exit) in
    curr := current_value;
    marked := current_mark;
  done;
  !curr.key = key && not !marked

(* Perform the operations on the linked list *)
let perform_operations linkedlist operations =
  List.iter (fun op ->
    let value = Random.int 10000 in
    match op with
    | 0 | 1 -> ignore (contains linkedlist value)
    | 2 -> ignore (removeitem linkedlist value)
    | _ -> ignore (additem linkedlist value)
  ) operations


let benchmark num_domains linkedlist operations_list =
  let barrier = create_barrier num_domains in
  let domains = List.init num_domains (fun i ->
    let operations = List.nth operations_list i in
    Domain.spawn (fun () ->
      await barrier;
      perform_operations linkedlist operations
    )
  ) in
  List.iter Domain.join domains
