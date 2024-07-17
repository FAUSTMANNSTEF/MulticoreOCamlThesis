open Approach.LockFree

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
  assert (not (removeitem linkedlist 5));
  assert (not (contains linkedlist 2));
  Printf.printf "Basic operations for LockFree test passed\n"

(* Test parallel operations on the list *)
let test_parallel () =
  let linkedlist = create_linkedlist () in
  let barrier = create_barrier 2 in
  let domainA = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 1);
    ignore (additem linkedlist 2);
    ignore (additem linkedlist 3);
    ignore (removeitem linkedlist 2);
    let value = contains linkedlist 5 in
    Printf.printf "The value 5 exists? %b\n" value
  ) in
  let domainB = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 4);
    ignore (additem linkedlist 5);
    ignore (additem linkedlist 6);
    ignore (removeitem linkedlist 5);
    ignore (removeitem linkedlist 3);
    ignore (removeitem linkedlist 1);
    let value2 = contains linkedlist 50 in
    Printf.printf "The value 50 exists? %b\n" value2;
    let value3 = contains linkedlist 4 in
    Printf.printf "The value 4 exists? %b\n" value3
  ) in
  Domain.join domainA;
  Domain.join domainB;
  print_list linkedlist

(* Executable to run the tests *)
let () =
  test_operations ();
  test_parallel ()
