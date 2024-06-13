let run_tests () =
  (* Create a new linked list *)
  let linkedlist = create_linkedlist () in
  
  (* Test 1: Add items to the linked list *)
  assert (add_item linkedlist 1);
  assert (add_item linkedlist 2);
  assert (add_item linkedlist 3);
  print_endline "Test 1 passed: Items added successfully.";
  
  (* Test 2: Check if items exist in the linked list *)
  assert (contains linkedlist 1);
  assert (contains linkedlist 2);
  assert (contains linkedlist 3);
  assert (not (contains linkedlist 4));
  print_endline "Test 2 passed: Contains function works correctly.";
  
  (* Test 3: Remove items from the linked list *)
  assert (remove_item linkedlist 2);
  assert (not (contains linkedlist 2));
  assert (contains linkedlist 1);
  assert (contains linkedlist 3);
  print_endline "Test 3 passed: Items removed successfully.";
  
  (* Test 4: Add and remove items, checking list consistency *)
  assert (add_item linkedlist 4);
  assert (contains linkedlist 4);
  assert (remove_item linkedlist 1);
  assert (not (contains linkedlist 1));
  assert (contains linkedlist 3);
  assert (contains linkedlist 4);
  print_endline "Test 4 passed: Add and remove operations work consistently.";
  
  (* Print the final state of the linked list *)
  print_endline "Final state of the linked list:";
  print_list linkedlist

(* Run the tests *)
let () = run_tests ()
