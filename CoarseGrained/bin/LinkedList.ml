
(** A node of our mutable linked list of type int *)
type 'a node = {
  value : 'a ;  
  mutable next : 'a node option;
}

(** The linked list itself *)
type 'a linkedlist = {
  mutable first : 'a node option ;
}

(** Create a new node with the given value *)
let create_node v = {
  value = v;
  next = None;
}
(**  linked list containing exactly one value *)
let make_linkedlist v = {
 first = Some (create_node v )
}

(**  linked list containing no value *)
let empty = {
  first = None;
    }
  
  (* Function to get a random index from a list *)
  let get_random_index len =
    Random.self_init ();
    if len = 0 then 
      0
  else
    Random.int len (*Generates random number from 0 to len-1*)
    
(* Function to insert a new node at a specific index in the linked list *)
let rec insert_at_index index new_value head =
  match index, head with
  | 0, _ ->  (* Insert at the head *)
    let new_node = create_node new_value in
    new_node.next <- head;
    Some new_node
  | _, None ->  (* Reach the end without finding the index *)
    failwith "Index out of bounds"
  | n, Some node when n > 0 ->  (* Traverse to the correct position *)
    node.next <- insert_at_index (n - 1) new_value node.next;
    Some node
  | _, _ -> failwith "Invalid index"
  
 (* Function to delete a node at a specific index in the linked list *)
 let rec delete_at_index index head =
  match index , head with
  | 0, Some node ->  (* Delete the head *)
    node.next
  | _, None ->  (* Reach the end without finding the index *)
    failwith "Index out of bounds"
  | n, Some node when n > 0 ->  (* Traverse to the correct position *)
    node.next <- delete_at_index (n - 1) node.next;
    Some node
  | _, _ -> failwith "Invalid index"

(* Utility function to print the linked list *)
let rec print_list = function
  | None -> print_string "None\n"
  | Some node ->
    Printf.printf "%d -> " node.value;
    print_list node.next

let linked_list = Some (create_node 3)
let linked_list = insert_at_index 0 5 linked_list
let linked_list = insert_at_index 1 8 linked_list
(* Utility function to get the length of a linked list *)
let rec length_of_list head count =
  match head with
  | None -> count
  | Some node -> length_of_list node.next (count + 1)

let () =
  let len = length_of_list linked_list 0 in
  let random_idx = get_random_index len in
  Printf.printf "Length of linked list 1 : %d\n" len;
  print_list linked_list;
  let linked_list = insert_at_index random_idx 80 linked_list in
  Printf.printf "Inserted 80 at random index %d\n" random_idx;
  print_string "Linked list contains: ";
  print_list linked_list;
  let len = length_of_list linked_list 0 in
  Printf.printf "Length of linked list 1 : %d\n" len;
  print_string "Linked list contains: ";
  print_list linked_list;
  let random_idx = 15 in
  Printf.printf "Random index generated: %d\n" random_idx;
  let linked_list = delete_at_index random_idx linked_list in
  Printf.printf "Deleted node at random index %d\n" random_idx;
  print_string "Linked list contains: ";
  print_list linked_list;
