(**Mli file for CoarseGrained.ml*)

(*Node for CoarseGrained linked list *)
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
(* Function to create a barrier for int threads *)
val create_barrier : int -> barrier
(*Barrier synchronization function*)
val await : barrier -> unit
(* Function to create a new empty linked list of type 'a *)
val create_linkedlist : unit -> int linkedlist
(* Function to add a new element to the linked list *)
val additem : 'a linkedlist -> 'a -> bool
(* Function to remove an element from the linked list *)
val removeitem : 'a linkedlist -> 'a  -> bool
(* Function to check if the linked list contains a specific element*)
val contains : 'a linkedlist -> 'a -> bool
(* Function to print linked list*)
val print_list : int linkedlist -> unit
(* Function to generate a list of random integers *)
val add_elements : 'a linkedlist -> 'a list -> unit
(* Function to generate a list of random operations *)
val generate_operations : int -> int list
(* Function to perform a list of operations on the linked list *)
val perform_operations : int linkedlist -> int list -> unit
(* Benchmark function to measure the execution time of parallel operations *)
val benchmark : int -> int list -> int -> unit