open Approach.CoarseGrained
(* Function to test basic linked list operations *)
let test_operations () =
  let linkedlist = create_linkedlist () in
  assert (additem linkedlist 1);
  assert (additem linkedlist 2);
  assert (additem linkedlist 3);
  assert (contains linkedlist 1);
  assert (contains linkedlist 2);
  assert (contains linkedlist 3);
  assert (removeitem linkedlist 2);
  assert (not (contains linkedlist 2));
  assert (not (removeitem linkedlist 5));
  Printf.printf "Basic operations for Coarse Grained test passed\n"

(* Function to test barrier synchronization *)
let test_barrier () =
  let barrier = create_barrier 2 in
  let counter = Atomic.make 0 in
  let domainA = Domain.spawn (fun () ->
    await barrier;
    Atomic.incr counter;
    Printf.printf "Domain A passed the barrier\n"
  ) in
  let domainB = Domain.spawn (fun () ->
    await barrier;
    Atomic.incr counter;
    Printf.printf "Domain B passed the barrier\n"
  ) in
  Domain.join domainA;
  Domain.join domainB;
  assert (Atomic.get counter = 2);
  Printf.printf "Barrier synchronization test passed\n"

(* Function to test parallel operations *)
let test_parallel () =
  let linkedlist = create_linkedlist () in
  let barrier = create_barrier 2 in
  let domainA = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 1);
    ignore (additem linkedlist 2);
    ignore (additem linkedlist 3);
    ignore (removeitem linkedlist 20);
    ignore (removeitem linkedlist 4);
    ignore (removeitem linkedlist 5);
    let value = contains linkedlist 1 in
    Printf.printf "Value 1 is in the list: %b\n" value
  ) in
  let domainB = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 4);
    ignore (additem linkedlist 5);
    ignore (additem linkedlist 6);
    ignore (removeitem linkedlist 15);
    ignore (additem linkedlist 8);
    ignore (removeitem linkedlist 3);
    ignore (removeitem linkedlist 2)
  ) in
  Domain.join domainA;
  Domain.join domainB;
  print_list linkedlist;
  Printf.printf "Parallel operations test passed\n"

(* Executable to run all tests *)
let () =
  test_operations ();
  test_barrier ();
  test_parallel ()