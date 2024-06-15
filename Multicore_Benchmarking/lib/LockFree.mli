(** The ATOMIC_MARKABLE module type defines the operations for atomic markable references. *)
module type ATOMIC_MARKABLE = sig
    type 'a markable
    (** The type of markable references. *)
  
    val markable_CAS : 'a markable -> 'a -> bool -> 'a -> bool -> bool
    (** [markable_CAS r exRef exMark newRef newMark] atomically sets the reference and mark in [r]
        to [newRef] and [newMark] if the current values are [exRef] and [exMark]. *)
  
    val get_reference : 'a markable -> 'a
    (** [get_reference r] returns the reference stored in [r]. *)
  
    val get_mark : 'a markable -> bool
    (** [get_mark r] returns the mark stored in [r]. *)
  
    val get_marked : 'a markable -> 'a * bool
    (** [get_marked r] returns a pair of the reference and the mark stored in [r]. *)
  
    val make_markable : 'a -> bool -> 'a markable
    (** [make_markable value mark] creates a new markable reference with the given [value] and [mark]. *)
  end
  
  (** The AtomicMarkable module implements the ATOMIC_MARKABLE interface. *)
  module AtomicMarkable : ATOMIC_MARKABLE
  
  (** A node in the linked list containing a value a key and a reference to the next node. *)
  type 'a node = { 
    value : 'a; 
    key : int; 
    next : 'a node AtomicMarkable.markable option; 
  }
  
  (** A barrier used to synchronize domains (threads). *)
  type barrier = { 
    waiters : int Atomic.t; 
    size : int; 
    passed : int Atomic.t; 
  }
  
  val make_node : 'a -> 'a node AtomicMarkable.markable option -> 'a node
  (** [make_node value next] creates a new node with the given [value] and reference to the [next] node. *)
  
  (** A linked list containing references to the first and last nodes. *)
  type 'a linkedlist = {
    mutable firstnode : 'a node;
    mutable lastnode : 'a node;
  }
  
  val create_linkedlist : unit -> int linkedlist
  (** [create_linkedlist ()] creates a new empty linked list with sentinel nodes. *)
  
  val create_barrier : int -> barrier
  (** [create_barrier size] creates a new barrier for synchronizing [size] domains. *)
  
  val await : barrier -> unit
  (** [await barrier] waits at the given [barrier] until all participating domains have arrived. *)
  
  val print_list : int linkedlist -> unit
  (** [print_list linkedlist] prints the contents of the given [linkedlist]. *)
  
  type 'a window = { 
    pred : 'a node; 
    curr : 'a node; 
  }
  
  val find_window : 'a linkedlist -> int -> 'a window
  (** [find_window linkedlist key] finds the window (predecessor and current node) for the given [key] in the [linkedlist]. *)
  
  val additem : 'a linkedlist -> 'a -> bool
  (** [additem linkedlist value] adds the given [value] to the [linkedlist] and returns true if successful, false if the value is already present. *)
  
  val removeitem : 'a linkedlist -> 'a -> bool
  (** [removeitem linkedlist value] removes the given [value] from the [linkedlist] and returns true if successful, false if the value is not found. *)

  val contains : 'a linkedlist -> 'a -> bool
  (** [contains linkedlist value] checks if the given [value] is present in the [linkedlist]. 
      Returns true if the value is found, false otherwise. *)
  
  val benchmark : int -> int linkedlist -> int list list -> unit
  (** [benchmark n linkedlist operations] runs a performance benchmark by applying the 
      given [operations] on the [linkedlist] [n] times. The [operations] parameter is a list 
      of lists of integers, where each inner list represents a series of operations to be 
      performed in sequence. *)
  