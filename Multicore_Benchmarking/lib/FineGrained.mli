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

(* Linked list for Fine Grained Synchronization *)
type 'a linkedlist = {
  mutable firstnode: 'a node; 
  (* The reason they are mutable is because they might point to different nodes throughout execution of the program, even though the values remain the same *)
  mutable lastnode: 'a node;
}

val create_barrier : int -> barrier
(** [create_barrier n] creates a barrier for [n] threads. *)

val await : barrier -> unit
(** [await barrier] synchronizes the threads at the given [barrier]. *)

val create_linkedlist : unit -> int linkedlist
(** [create_linkedlist ()] creates a new empty linked list of type ['a]. *)

val additem : 'a linkedlist -> 'a -> bool
(** [additem linkedlist value] inserts the given [value] in the [linkedlist] if it is not already there. Returns true if successful, false otherwise. *)

val removeitem : 'a linkedlist -> 'a -> bool
(** [removeitem linkedlist value] removes the given [value] from the [linkedlist] if it is present. Returns true if successful, false otherwise. *)

val contains : 'a linkedlist -> 'a -> bool
(** [contains linkedlist value] checks if the given [value] is contained in the [linkedlist]. Returns true if the value is found, false otherwise. *)

val print_list : int linkedlist -> unit
(** [print_list linkedlist] prints the elements of the [linkedlist]. *)

val perform_operations : int linkedlist -> int list -> unit
(** [perform_operations linkedlist operations] performs a list of [operations] on the [linkedlist]. *)

val benchmark : int -> int linkedlist -> int list list -> unit
(** [benchmark n linkedlist operations] runs a performance benchmark by applying the 
    given [operations] on the [linkedlist] [n] times. The [operations] parameter is a list 
    of lists of integers, where each inner list represents a series of operations to be 
    performed in sequence. *)
