(* Define a type for nodes in a linked list *)
type 'a node = {
  value : 'a;
  key : int;
  mutable next : 'a node option;
}

(* Linked list for Coarse Grained Synchronization*)
type 'a linkedlist = {
  mutable firstnode: 'a node; (* The reason they are mutable is because the might point to different nodes throughout execution of the programm, even though the values remain the same *)
  mutable lastnode: 'a node;
}

(* Function to create a new empty linked list with sentinel nodes *)
let create_linkedlist () : 'a linkedlist =
  let sentinel1 = { value = min_int; key =  min_int; next = None } in
  let sentinel2 = { value = max_int; key =  max_int; next = None } in
  sentinel1.next <- Some sentinel2;
  {
    firstnode = sentinel1;
    lastnode = sentinel2;
  }

(* Function to add a new item to the linked list if not there *)
let additem linkedlist value =
  let key = Hashtbl.hash value in
  let rec find_insertion_point pred curr_opt =
    match curr_opt with
    (* If current key is less than the key of the value to be inserted, continue to the next node *)
    | Some curr when curr.key < key ->
      find_insertion_point curr curr.next
    (* If the key of the value to be inserted is already in the list, return false *)
    | Some curr when curr.key = key ->
      false
    (* If the key of the value to be inserted is greater than the current key, insert the new node *)
    | curr_opt ->
      let new_node = { value = value; key = key; next = curr_opt } in
      pred.next <- Some new_node;
      true
  in
  find_insertion_point linkedlist.firstnode linkedlist.firstnode.next

(* Function to remove an item from the linked list if it is there *)
let removeitem linkedlist value =
  let key = Hashtbl.hash value in
  let rec find_remove_point pred curr_opt =
    match curr_opt with
    (* If current key is less than the key of the value to be removed, continue to the next node *)
    | Some curr when curr.key < key ->
      find_remove_point curr curr.next
    (* If the key of the value to be removed is in the list,remove *)
    | Some curr when curr.key = key ->
      pred.next <- curr.next;
      true
    (* If the key of the value to be removed is not in the list, return false *)
    | _ ->
      false
  in
  find_remove_point linkedlist.firstnode linkedlist.firstnode.next
(* Function to check if an item exists in the list*)
let contains linkedlist value =
  let key = Hashtbl.hash value in
  let rec find_point curr_opt =
    match curr_opt with
    (* If current key is less than the key of the value to be removed, continue to the next node *)
    | Some curr when curr.key < key ->
      find_point curr.next
    (* If the key of the value to be removed is in the list,remove *)
    | Some curr when curr.key = key ->
      true
    (* If the key of the value to be removed is not in the list, return false *)
    | _ ->
      false
  in
  find_point linkedlist.firstnode.next

(* Function to print the linked list *)
let print_listvalues linkedlist =
  let rec print_node = function
    | None -> ()
    | Some n ->
      Printf.printf "%d " n.value;

      print_node n.next
  in
  print_node (Some linkedlist.firstnode);
  print_newline ()

let print_listkeys linkedlist =
  let rec print_node = function
    | None -> ()
    | Some n ->
      Printf.printf "%d " n.key;
      print_node n.next
  in
  print_node (Some linkedlist.firstnode);
  print_newline ()
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
  for i = 0 to num_domains - 1 do
    let operations = List.nth operations_list i in
    perform_operations linkedlist operations;
  done