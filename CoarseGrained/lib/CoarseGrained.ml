(* Define a type for nodes in a linked list *)
type 'a node = {
  value : 'a;
  key : int;
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
  lock: Mutex.t;
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


(* Function to create a new empty linked list with sentinel nodes *)
let create_linkedlist () : 'a linkedlist =
  let sentinel1 = { value = min_int; key = Hashtbl.hash min_int; next = None } in
  let sentinel2 = { value = max_int; key = Hashtbl.hash max_int; next = None } in
  sentinel1.next <- Some sentinel2;
  {
    firstnode = sentinel1;
    lastnode = sentinel2;
    lock = Mutex.create ();
  }

(* Function to add a new item to the linked list if not there *)
let additem linkedlist value =
  Mutex.lock linkedlist.lock;
  let key = Hashtbl.hash value in
  let rec find_insertion_point pred curr_opt =
    match curr_opt with
    (* If current key is less than the key of the value to be inserted, continue to the next node *)
    | Some curr when curr.key < key ->
      find_insertion_point curr curr.next
    (* If the key of the value to be inserted is already in the list, return false *)
    | Some curr when curr.key = key ->
      Mutex.unlock linkedlist.lock;
      false
    (* If the key of the value to be inserted is greater than the current key, insert the new node *)
    | curr_opt ->
      let new_node = { value = value; key = key; next = curr_opt } in
      pred.next <- Some new_node;
      Mutex.unlock linkedlist.lock;
      true
  in
  find_insertion_point linkedlist.firstnode linkedlist.firstnode.next

(* Function to remove an item from the linked list if it is there *)
let removeitem linkedlist value  =
  Mutex.lock linkedlist.lock;
  let key = Hashtbl.hash value in
  let rec find_remove_point pred curr_opt =
    match curr_opt with
    (* If current key is less than the key of the value to be removed, continue to the next node *)
    | Some curr when curr.key < key ->
      find_remove_point curr curr.next
    (* If the key of the value to be removed is in the list,remove *)
    | Some curr when curr.key = key ->
      pred.next <- curr.next;
      Mutex.unlock linkedlist.lock;
      true
    (* If the key of the value to be removed is not in the list, return false *)
    | _ ->
      Mutex.unlock linkedlist.lock;
      false
  in
  find_remove_point linkedlist.firstnode linkedlist.firstnode.next
  
(*Function that checks if the value exists in the linked list *)
let contains linkedlist value =
  Mutex.lock linkedlist.lock;
  let key = Hashtbl.hash value in
  let rec find_point curr_opt =
    match curr_opt with
    | Some curr when curr.key < key ->
      find_point curr.next
    | Some curr when curr.key = key ->
      Mutex.unlock linkedlist.lock;
      true
    | _ ->
      Mutex.unlock linkedlist.lock;
      false
  in
  find_point linkedlist.firstnode.next
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
    ignore (removeitem linkedlist 20);
    ignore (removeitem linkedlist 4);
    ignore (removeitem linkedlist 5);
    let value = contains linkedlist 1 in
    Printf.printf "Value 1 is in the list: %b\n" value
  ) in
  let domainB = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 4 );
    ignore (additem linkedlist 5 );
    ignore (additem linkedlist 6 );
    ignore (removeitem linkedlist 15);
    ignore (additem linkedlist 8);
    ignore (removeitem linkedlist 3 );
    ignore (removeitem linkedlist 2 )
  ) in
  Domain.join domainA;
  Domain.join domainB;
  print_list linkedlist

(** Executes testparallel by default *)
let () = testparallel ()
