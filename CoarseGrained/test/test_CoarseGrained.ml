open Approach.CoarseGrained
(* Test function to initialize and manipulate the linked list *)
(* let testaddition () =
  let linkedlist = create_linkedlist () in
  let node1 = {value = min_int; key = Hashtbl.hash min_int; next = None} in
  linkedlist.firstnode <- Some node1;
  ignore (additem linkedlist max_int);  
  ignore (additem linkedlist 5);
  ignore (additem linkedlist 8);
  ignore (additem linkedlist 5);
  print_list linkedlist.firstnode;
  ignore (removeitem linkedlist 5);
  print_list linkedlist  Print the list *)

(* Test parallel operations on the list *)
let test_parallel () =
  let linkedlist = create_linkedlist () in
  let barrier = create_barrier 2 in
  let domainA = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 1 );
    ignore (additem linkedlist 2 );
    ignore (additem linkedlist 3 );
    ignore (removeitem linkedlist 20);
    ignore (removeitem linkedlist 4);
    ignore (removeitem linkedlist 5);
    let value = contains linkedlist 1 in
    Printf.printf "Value 1 is in the list: %b\n" value
  ) in
  let domainB = Domain.spawn (fun () ->
    await barrier;
    ignore (additem linkedlist 4 );
    ignore (additem linkedlist 5 );
    ignore (additem linkedlist 6 );
    ignore (removeitem linkedlist 15);
    ignore (additem linkedlist 8);
    ignore (removeitem linkedlist 3 );
    ignore (removeitem linkedlist 2 )
  ) in
  Domain.join domainA;
  Domain.join domainB;
  print_list linkedlist

let () = test_parallel ()