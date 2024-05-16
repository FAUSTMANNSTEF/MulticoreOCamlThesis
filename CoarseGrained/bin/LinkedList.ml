(* Define a type for nodes in a linked list *)
type 'a node = {
  value : 'a;
  key : int;
  mutable next : 'a node option;
  }
  
type barrier = {
    waiters : int Atomic.t;
    size : int;
    passed : int Atomic.t
    }
    
    (* Create a new barrier *)
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

(* Define a type for the linked list itself, including a mutex for thread safety *)
type 'a linkedlist = {
  mutable firstnode: 'a node option;
  lock: Mutex.t;
}

(* Function to create a new, empty linked list *)
let create_linkedlist () : 'a linkedlist = {
  firstnode = None;  (* Start with no nodes *)
  lock = Mutex.create ();
}

let additem linkedlist value barrier =
  await barrier;
    Mutex.lock linkedlist.lock;
    let key = Hashtbl.hash value in
    let rec find_insertion_point pred_opt curr_opt =
      match (pred_opt, curr_opt) with
      | (Some pred, Some curr) when curr.key < key ->
        find_insertion_point (Some curr) curr.next
      | (Some pred, Some curr) when curr.key = key ->
        Mutex.unlock linkedlist.lock;
        false
      | (Some pred, curr_opt) ->
        let new_node = {value = value; key = key; next = curr_opt} in
        pred.next <- Some new_node;
        Mutex.unlock linkedlist.lock;
        true
      (**Last 2 pattern matches might not be needed*)  
      | (None, Some curr) when curr.key < key ->
        find_insertion_point (Some curr) curr.next
      | (None, curr_opt) ->
        let new_node = {value = value; key = key; next = curr_opt} in
        linkedlist.firstnode <- Some new_node;
        Mutex.unlock linkedlist.lock;
        true
      | _ -> assert false
    in
    find_insertion_point None linkedlist.firstnode

let removeitem linkedlist value barrier =
  await barrier;
    Mutex.lock linkedlist.lock;
    let key = Hashtbl.hash value in
    let rec find_and_remove pred_opt curr_opt =
      match (pred_opt, curr_opt) with
      | (Some pred, Some curr) when curr.key < key ->
        find_and_remove (Some curr) curr.next
      | (Some pred, Some curr) when curr.key = key ->
        pred.next <- curr.next;
        Mutex.unlock linkedlist.lock;
        true
      | (Some pred, curr_opt) ->
        Mutex.unlock linkedlist.lock;
        false
      | (None, Some curr) when curr.key < key ->
        find_and_remove (Some curr) curr.next
      | (None, Some curr) ->
        linkedlist.firstnode <- curr.next;
        Mutex.unlock linkedlist.lock;
        true
      | _ -> assert false
    in
    find_and_remove None linkedlist.firstnode


 
(* Function to print the linked list *)
let print_list head =
  let rec print_list_helper = function  (* Helper function using pattern matching *)
    | None -> ()  (* End of the list *)
    | Some node ->
      Printf.printf " Node %d " node.value;
      print_list_helper node.next  (* Recursive call to print the next node *)
  in
  print_list_helper head;  (* Start the recursive printing *)
  print_newline ()

  (* Test function to initialize and manipulate the linked list *)
(* let test () =
  let linkedlist = create_linkedlist () in
  let node1 = {value = min_int; key = Hashtbl.hash min_int; next = None} in
  linkedlist.firstnode <- Some node1;
  ignore (additem linkedlist max_int barrier);  
  ignore (additem linkedlist 5 barrier);
  ignore (additem linkedlist 8 barrier);
  ignore (additem linkedlist 5 barrier);
  print_list linkedlist.firstnode;
  ignore (removeitem linkedlist 5 barrier);
  print_list linkedlist.firstnode  Print the list *)

let testparallel ()=
  let linkedlist = create_linkedlist () in
  let node1 = {value = min_int; key = Hashtbl.hash min_int; next = None} in
  linkedlist.firstnode <- Some node1;
  let barrier = create_barrier 2 in
  let domainA = Domain.spawn (fun () -> 
    ignore (additem linkedlist 1 barrier); 
    ignore (additem linkedlist 2 barrier);
    ignore (additem linkedlist 3 barrier);
    ignore (removeitem linkedlist 20 barrier);
    ignore (removeitem linkedlist 2 barrier)
  ) in
  let domainB = Domain.spawn (fun () -> 
    ignore (additem linkedlist 4 barrier);
    ignore (additem linkedlist 5 barrier);
    ignore (additem linkedlist 6 barrier);
    ignore (removeitem linkedlist 15 barrier);
    ignore (removeitem linkedlist 1 barrier)
  ) in
  Domain.join domainA;
  Domain.join domainB;
  print_list linkedlist.firstnode
  
(* Execute the test function *)
let () = testparallel ()

