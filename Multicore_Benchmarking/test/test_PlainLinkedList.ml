open Approach.PlainLinkedList

(* Test sequential operations on the list *)
let testadditiondeletioncontains () =
  let linkedlist = create_linkedlist () in
  ignore (additem linkedlist 1);
  ignore (additem linkedlist 5);
  ignore (additem linkedlist 3);
  ignore (additem linkedlist 2);
  ignore (removeitem linkedlist 4);
  ignore (removeitem linkedlist 3);
  ignore (additem linkedlist 1);
  let value = contains linkedlist 1 in
  Printf.printf "Is 1 in the list %b\n" value;
  let value3= contains linkedlist 3 in
  Printf.printf "Is 3 in the list %b\n" value3;
  let value10 = contains linkedlist 10 in
  Printf.printf "Is 10 in the list %b\n" value10;
  print_listvalues linkedlist

(* Test if nodes are in ascending order on the list *) 
let testkeysinascedingorder () =
  let linkedlist = create_linkedlist () in
  ignore (additem linkedlist 1);
  ignore (additem linkedlist 2);
  ignore (additem linkedlist 3);
  ignore (additem linkedlist 4);
  ignore (removeitem linkedlist 4);
  ignore (additem linkedlist 5);
  print_listkeys linkedlist

(* Executable to run the tests *)  
let () =
  testadditiondeletioncontains ();
  testkeysinascedingorder ()