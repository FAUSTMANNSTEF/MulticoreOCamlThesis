(* Define a type for nodes in a linked list *)
type 'a node = {
  value : 'a;
  key : int;
  lock: Mutex.t;
  mutable next : 'a node option;
  }
  (* Barrier used to force threads to start working at the same time *)
type barrier = {
  waiters : int Atomic.t;
  size : int;
  passed : int Atomic.t
  }
    (* Linked list for Coarse Grained Synchronization*)
type 'a linkedlist = {
  mutable firstnode: 'a node; (* The reason they are mutable is because the might point to different nodes throughout execution of the programm, even though the values remain the same *)
  mutable lastnode: 'a node;
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


(* Function to create a new, empty linked list with sentinel nodes *)
let create_linkedlist () : 'a linkedlist =
  let sentinel1 = { value = min_int; key = Hashtbl.hash min_int; lock = Mutex.create (); next = None } in
  let sentinel2 = { value = max_int; key = Hashtbl.hash max_int; lock = Mutex.create (); next = None } in
  sentinel1.next <- Some sentinel2;
  {
    firstnode = sentinel1;
    lastnode = sentinel2;
  }
(* Function to add a new item to the linked list if not there *)
let additem linkedlist value =
    let key = Hashtbl.hash value in
    let pred = linkedlist.firstnode in
    Mutex.lock pred.lock;
    let rec find_and_insert pred curr_opt =
      match curr_opt with
      | Some curr ->
        Mutex.lock curr.lock;
        if curr.key < key then (
          Mutex.unlock pred.lock;
          find_and_insert curr curr.next
        ) else if curr.key = key then (
          Mutex.unlock curr.lock;
          Mutex.unlock pred.lock;
          false
        ) else (
          let new_node = { value = value; key = key; next = curr_opt; lock = Mutex.create () } in
          pred.next <- Some new_node;
          Mutex.unlock curr.lock;
          Mutex.unlock pred.lock;
          true
        )
      | None -> 
        Mutex.unlock pred.lock;
        false
    in
    find_and_insert pred pred.next

let removeitem linkedlist value =
    let key = Hashtbl.hash value in
    let pred = linkedlist.firstnode in
    Mutex.lock pred.lock;
    let rec find_and_remove pred curr_opt =
      match curr_opt with
      | Some curr ->
        Mutex.lock curr.lock;
        if curr.key < key then (
          Mutex.unlock pred.lock;
          find_and_remove curr curr.next
        ) else if curr.key = key then (
          pred.next <- curr.next;
          Mutex.unlock curr.lock;
          Mutex.unlock pred.lock;
          true
        ) else (
          Mutex.unlock curr.lock;
          Mutex.unlock pred.lock;
          false
        )
      | None -> 
        Mutex.unlock pred.lock;
        false
    in
    find_and_remove pred pred.next
    
(* Function to check if a value is in the linked list *)
let contains linkedlist value =
    let key = Hashtbl.hash value in
    let pred = linkedlist.firstnode in
    Mutex.lock pred.lock;
    let rec find pred curr_opt =
      match curr_opt with
      | Some curr ->
        Mutex.lock curr.lock;
        if curr.key < key then (
          Mutex.unlock pred.lock;
          find curr curr.next
        ) else if curr.key = key then (
          Mutex.unlock curr.lock;
          Mutex.unlock pred.lock;
          true
        ) else (
          Mutex.unlock curr.lock;
          Mutex.unlock pred.lock;
          false
        )
      | None -> 
        Mutex.unlock pred.lock;
        false
    in
    find pred pred.next

(* Function to print the linked list *)
let print_list linkedlist =
  let rec print_node = function
    | None -> ()
    | Some n ->
      Printf.printf "%d " n.value;
      print_node n.next
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
    ignore (removeitem linkedlist 2 );
    ignore (removeitem linkedlist 4 );
    let value=contains linkedlist 5 in
    Printf.printf "The value 5 exists ? %b \n" value
  ) in
  let domainB = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 4 );
    ignore (additem linkedlist 5 );
    ignore (additem linkedlist 6 );
    ignore (removeitem linkedlist 5);
    ignore (removeitem linkedlist 3);
    ignore (removeitem linkedlist 1);
    let value2=contains linkedlist 50 in
    Printf.printf "The value 50 exists? %b \n" value2;
    let value3=contains linkedlist 4 in
    Printf.printf "The value 4 exists? %b \n" value3
  ) in
  Domain.join domainA;
  Domain.join domainB;
  print_list linkedlist

(** Executes testparallel by default *)
(* let () = testparallel () *)
  
let hello () =
  print_endline "Hello, world!"
