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
(*Creates a new barrier*)    
val create_barrier : int -> barrier
(* Barrier synchronization function*)
val await : barrier -> unit
(*Creates a new linked list*)
val create_linkedlist : unit -> int linkedlist
(*Inserts a new node in the linked list if not there*)
val additem : 'a linkedlist -> 'a  -> bool
(*Removes a node from the linked list if it is not there*)
val removeitem : 'a linkedlist -> 'a  -> bool
(*Function to check if a value is contained in the linked list*)
val contains : 'a linkedlist -> 'a -> bool
(*Prints linked list*)
val print_list : int linkedlist -> unit
(*Tests parallel operations on the list*)
val testparallel : unit -> unit
val hello : unit -> unit