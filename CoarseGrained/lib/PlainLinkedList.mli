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
val create_linkedlist : unit -> int linkedlist
(* Function to add a new item to the linked list if not there *)
val additem : 'a linkedlist -> 'a -> bool
(* Function to remove an item from the linked list *)
val removeitem : 'a linkedlist -> 'a -> bool
(* Function to check if an item is in the linked list *)
val contains : 'a linkedlist -> 'a -> bool
(* Function to print the linked list *)
val print_listvalues : int linkedlist -> unit
(* Function to check if an item is in the linked list *)
val print_listkeys : 'a linkedlist -> unit
(* Function to generate a random list of integers *)
val generate_random_list : int -> int list
(*Function to add elements in the linked list*)
val add_elements: 'a linkedlist -> 'a list -> unit
(*Function to generate operations*)
val generate_operations: int -> int list
(*Function to perform operations*)
val perform_operations: int linkedlist -> int list-> unit
(*Function to check if the keys are in ascending order*)
(* Function to check if an item is in the linked list *)
(* Test addition and deletion on the list*)
val testadditiondeletioncontains : unit -> unit
(* Test keys are in ascending order*)
val testkeysinascedingorder : unit -> unit
(*Test with arbitrary amount of lists*)
val test_with_n_list_operations : int -> unit