(* Define a type for nodes in a linked list *)
type 'a node = {
  value : 'a;
  key : int;
  mutable next : 'a node option;
}

(* Linked list for Plain Linked List Synchronization *)
type 'a linkedlist = {
  mutable firstnode: 'a node; 
  (* The reason they are mutable is because they might point to different nodes throughout execution of the program, even though the values remain the same *)
  mutable lastnode: 'a node;
}

val create_linkedlist : unit -> int linkedlist
(** [create_linkedlist ()] creates a new empty linked list with sentinel nodes. *)

val additem : 'a linkedlist -> 'a -> bool
(** [additem linkedlist value] adds the given [value] to the [linkedlist] if it is not already there. Returns true if successful, false otherwise. *)

val removeitem : 'a linkedlist -> 'a -> bool
(** [removeitem linkedlist value] removes the given [value] from the [linkedlist] if it is present. Returns true if successful, false otherwise. *)

val contains : 'a linkedlist -> 'a -> bool
(** [contains linkedlist value] checks if the given [value] is in the [linkedlist]. Returns true if the value is found, false otherwise. *)

val print_listvalues : int linkedlist -> unit
(** [print_listvalues linkedlist] prints the values of the elements in the [linkedlist]. *)

val print_listkeys : 'a linkedlist -> unit
(** [print_listkeys linkedlist] prints the keys of the elements in the [linkedlist]. *)

val perform_operations: int linkedlist -> int list -> unit
(** [perform_operations linkedlist operations] performs a list of [operations] on the [linkedlist]. *)

val benchmark : int -> int linkedlist -> int list list -> unit
(** [benchmark n linkedlist operations] runs a performance benchmark by applying the 
    given [operations] on the [linkedlist] [n] times. The [operations] parameter is a list 
    of lists of integers, where each inner list represents a series of operations to be 
    performed in sequence. *)
