type queue = {
  mutable lst : int list;
  lock : Mutex.t
}
type barrier = {
  waiters : int Atomic.t;
  size : int;
  passed : int Atomic.t
}

(* Create a new queue with an associated mutex *)
let create_queue () : queue = {
  lst = [];
  lock = Mutex.create ()
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

let push q a id barrier =
  await barrier;
  Mutex.lock q.lock;
  q.lst <- a :: q.lst;  (* This prepends the element. *)
  Printf.printf "Thread %d pushed\n" id;  (* Corrected line for printing the message with ID *)
  Mutex.unlock q.lock
(* Function to push random elements into the queue *)
let push_random_elements id q num_elements barrier =
  for _ = 1 to num_elements do
    let random_element = Random.int 100  (* Generates a random integer between 0 and 99 *)
    in push q random_element id barrier
done

(* Set up two domains to push random elements into the queue *)
let test_random_pushes num_elements_per_domain =
  let q = create_queue () in
  let barrier = create_barrier 2 in
  let domainA = Domain.spawn (fun () -> push_random_elements 1 q num_elements_per_domain barrier) in
  let domainB = Domain.spawn (fun () -> push_random_elements 2 q num_elements_per_domain barrier) in
  print_string "Both domains spawned";
  Domain.join domainA;
  Domain.join domainB;
  (List.length q.lst, q.lst)

(* Initialize random seed *)
let () = Random.self_init ()

(* Call test function and print results *)
let () =
  let (len, contents) = test_random_pushes 5 in
  Printf.printf "Queue length: %d\nContents: %s\n" len (String.concat ", " (List.map string_of_int contents))


