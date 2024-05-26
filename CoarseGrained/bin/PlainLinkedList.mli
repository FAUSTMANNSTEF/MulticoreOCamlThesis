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
(* Function to print the linked list *)
val print_listvalues : int linkedlist -> unit
(* Function to check if an item is in the linked list *)
val print_listkeys : 'a linkedlist -> unit
(* Test addition and deletion on the list*)
val testadditiondeletion : unit -> unit
(* Test keys are in ascending order*)
val testkeysinascedingorder : unit -> unit