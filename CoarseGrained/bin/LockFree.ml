open Atomic
exception ItemExists
exception ItemAdded
exception ItemNotFound
exception ItemRemoved
(* utop path for me #use "/mnt/d/Groningen/Year 3/Thesis/Coding/MulticoreOCamlThesis/CoarseGrained/bin/LockFree.ml";; *)
type 'a node

(*Atomic markable reference function used to refference next node *)
type 'a atomic_mark_ref = ('a node option * bool) Atomic.t

(*Node structure in linked list*)
and 'a node = {
  value : 'a;
  key : int;
  next : 'a atomic_mark_ref ;
}
(* Function to create a new atomic markable reference *)
let make_atomic_mark_ref value mark = 
  Atomic.make (value, mark)
  
(* Function to create a new node with a given value *)
let make_node value next = 
  let key = Hashtbl.hash value in
  { value = value; key = key; next = next }
  
(* Linked list*)
type 'a linkedlist = {
  mutable firstnode: 'a node; (* The reason they are mutable is because the might point to different nodes throughout execution of the programm, even though the values remain the same *)
  mutable lastnode: 'a node;
  }


(* Barrier used to force threads to start working at the same time *)
type barrier = {
    waiters : int Atomic.t;
    size : int;
    passed : int Atomic.t
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

(* Function to create a new linked list *)
let create_linkedlist () : 'a linkedlist =
  let sentinel2 = make_node max_int (make_atomic_mark_ref None false) in
  let sentinel1 = make_node min_int (make_atomic_mark_ref (Some sentinel2) false) in
  {
    firstnode = sentinel1;
    lastnode = sentinel2;
  }
 
(* Function to replicate attempt_mark functionality in java from AtomicMarkableReference class
let attempt_mark current expected_node new_mark =
  let current_value,_ = Atomic.get current.ref in
  match current_value with
  | Some node when node = expected_node ->
    Atomic.set current.ref (Some expected_node, new_mark);
    true
  | _ -> false  *)

(*Type window used for return value of find_window function*)
type 'a window = {
    pred: 'a node ref;
    curr: 'a node ref;
  }
(* Function aimed to replicate find method in book *)
let find_window linkedlist key =
  let rec retry () =
    let pred = ref linkedlist.firstnode in
    let curr = ref (Option.get (fst (Atomic.get !pred.next))) in
    let marked = [|false|] in
    let succ = ref None in
    let continue_traversal = ref true in
    while !continue_traversal do
      try
        while true do
          let current_value, current_mark = Atomic.get !curr.next in
          marked.(0) <- current_mark;
          succ := current_value;
          while marked.(0) do
            let snip = Atomic.compare_and_set !pred.next (Some !curr,false ) (!succ,false) in
            if not snip then raise Exit;
            curr := Option.get !succ;
            let current_value, current_mark = Atomic.get !curr.next in
            marked.(0) <- current_mark;
            succ := current_value;
          done;
          if !curr.key >= key then (
            continue_traversal := false;
            raise Exit
          );
          pred := !curr;
          curr := Option.get !succ;
        done
      with Exit ->
        continue_traversal := false
    done;
    { pred; curr }  (* Return the references themselves *)
  in
  retry ()
  
(* Add item to the linked list, using references to the same physical addresses *)
let additem linkedlist value =
  let key = Hashtbl.hash value in
  let rec loop () =
    let window = find_window linkedlist key in
    let pred = window.pred in
    let curr = window.curr in
    if !curr.key = key then begin
      raise ItemExists
    end else begin
      let node = make_node value (make_atomic_mark_ref (Some !curr) false) in
      Printf.printf "Trying CAS:\n";
      Printf.printf "Pred.next key: %s, %b\n"
        (match fst (Atomic.get !pred.next) with
         | Some n -> string_of_int n.key 
         | None -> "None")
        (snd (Atomic.get !pred.next));
      Printf.printf "Expected curr key: %d\n" !curr.key;
      Printf.printf "New node key: %d\n" node.key;

      (* Debugging: Print memory addresses *)
      Printf.printf "Pred address: %d\n" (Obj.magic pred : int);
      Printf.printf "Curr address: %d\n" (Obj.magic curr : int);
      Printf.printf "Node address: %d\n" (Obj.magic node : int);
      Printf.printf "Pred.next address: %d\n" (Obj.magic !pred.next : int);
      Printf.printf "Curr.next address: %d\n" (Obj.magic !curr.next : int);
      Printf.printf "Node.next address: %d\n" (Obj.magic node.next : int);

      if Atomic.compare_and_set !pred.next (Some !curr, false) (Some node, false) then begin
        Printf.printf "Item added\n";
        raise ItemAdded
      end else begin
        Printf.printf "CAS failed, retrying...\n";
      end
    end;
    loop ()
  in
  try
    loop (); false
  with
  | ItemExists -> false
  | ItemAdded -> true

(* let removeitem linkedlist value =
  let key = Hashtbl.hash value in
  let rec loop () =
    let window = find_window linkedlist key in
    let pred = window.pred in
    let curr = window.curr in
    if curr.key <> key then
      raise ItemNotFound
    else begin
      let succ = Option.get (fst (Atomic.get curr.next.ref)) in
      let snip = attempt_mark curr.next succ true in
      if not snip then
        loop ()
      else begin
        ignore (compare_and_set pred.next (Some curr) false (Some succ) false);
        raise ItemRemoved
      end
    end
  in
  try
    loop (); false
  with
  | ItemNotFound -> false
  | ItemRemoved -> true *)
   
(* Function to print the linked list *)
let print_list linkedlist =
  let rec print_node = function
    | None -> ()
    | Some n ->
      Printf.printf "%d " n.value;
      print_node (fst (Atomic.get n.next))
  in
  print_node (Some linkedlist.firstnode);
  print_newline ()

(* Test parallel operations on the list *)
let testparallel () =
  let linkedlist = create_linkedlist () in
  let barrier = create_barrier 2 in
  let domainA = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 1 );
    ignore (additem linkedlist 2 );
    ignore (additem linkedlist 3 );
  ) in
  let domainB = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 4 );
    ignore (additem linkedlist 5 );
    ignore (additem linkedlist 6 );
  ) in
  Domain.join domainA;
  Domain.join domainB;
  print_list linkedlist
(* Test single domain operations on the list *)
let test_single_domain () =
  let linkedlist = create_linkedlist () in
  let barrier = create_barrier 1 in
  let domainA = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 1);
    ignore (additem linkedlist 2);
    ignore (additem linkedlist 3);
    ignore (additem linkedlist 4);
    ignore (additem linkedlist 5);
    ignore (additem linkedlist 6);
  ) in
  Domain.join domainA;
  print_list linkedlist

let testadditem () =
   let linkedlist = create_linkedlist () in
   let find = find_window linkedlist 1 in
   let pred = find.pred in
   let curr = find.curr in
   let sentinel1 = linkedlist.firstnode in
   let sentinel2 = linkedlist.lastnode in
   let value = 1 in
   let node = make_node value (make_atomic_mark_ref (Some !curr) false) in
   
   let pred_next = Atomic.get !pred.next in
   (* Print the details before compare_and_set *)
   Printf.printf "Before CAS:\n";
   Printf.printf "Pred.next: (%s, %b)\n" 
     (match fst pred_next with 
      | Some n -> string_of_int n.key 
      | None -> "None") 
     (snd pred_next);
   Printf.printf "Curr: %d\n" !curr.key;
   Printf.printf "Node: %d\n" node.key;
   Printf.printf "Sentinel1:%d\n" sentinel1.key;
   Printf.printf "Sentinel2:%d\n" sentinel2.key;
   (* Print the addresses *)
   Printf.printf "Pred address: %d\n" (Obj.magic pred : int);
   Printf.printf "Curr address: %d\n" (Obj.magic curr : int);
   Printf.printf "Node address: %d\n" (Obj.magic node : int);
   Printf.printf "Pred.next address: %d\n" (Obj.magic !pred.next : int);
   Printf.printf "Curr.next address: %d\n" (Obj.magic !curr.next : int);
   Printf.printf "Node.next address: %d\n" (Obj.magic node.next : int);
   Printf.printf "Sentinel1 address: %d\n" (Obj.magic sentinel1 : int);
   Printf.printf "Sentinel2 address: %d\n" (Obj.magic sentinel2 : int);
   
   
   (* Perform the compare_and_set operation *)
   if Atomic.compare_and_set !pred.next (Some !curr, false) (Some node, false) then
     Printf.printf "Item added\n"
   else
     Printf.printf "Item not added\n";
   
   (* Print the details after compare_and_set *)
   let pred_next_after = Atomic.get !pred.next in
   Printf.printf "After CAS:\n";
   Printf.printf "Pred.next: (%s, %b)\n" 
     (match fst pred_next_after with 
      | Some n -> string_of_int n.key 
      | None -> "None") 
     (snd pred_next_after);
   
   (* Print the addresses *)
   Printf.printf "Pred address: %d\n" (Obj.magic pred : int);
   Printf.printf "Curr address: %d\n" (Obj.magic curr : int);
   Printf.printf "Node address: %d\n" (Obj.magic node : int);
   Printf.printf "Pred.next address: %d\n" (Obj.magic !pred.next : int);
   Printf.printf "Curr.next address: %d\n" (Obj.magic !curr.next : int);
   Printf.printf "Node.next address: %d\n" (Obj.magic node.next : int);
   Printf.printf "Sentinel1 address: %d\n" (Obj.magic sentinel1 : int);
   Printf.printf "Sentinel2 address: %d\n" (Obj.magic sentinel2 : int);
   Printf.printf "No more infinite loop\n" 
 
 (* Call the test function *)
 let () = testadditem ()
  