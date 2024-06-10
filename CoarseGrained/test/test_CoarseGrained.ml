  Test function to initialize and manipulate the linked list
let testaddition () =
  let linkedlist = create_linkedlist () in
  let node1 = {value = min_int; key = Hashtbl.hash min_int; next = None} in
  linkedlist.firstnode <- Some node1;
  ignore (additem linkedlist max_int);  
  ignore (additem linkedlist 5);
  ignore (additem linkedlist 8);
  ignore (additem linkedlist 5);
  print_list linkedlist.firstnode;
  ignore (removeitem linkedlist 5);
  print_list linkedlist  (* Print the list *)

 (* Test parallel operations on the list *)
let testparallel () =
  let linkedlist = create_linkedlist () in
  let barrier = create_barrier 2 in
  let domainA = Domain.spawn (fun () ->
    ignore (additem linkedlist 1 barrier);
    ignore (additem linkedlist 2 barrier);
    ignore (additem linkedlist 3 barrier);
    ignore (removeitem linkedlist 20 barrier);
    ignore (removeitem linkedlist 2 barrier)
  ) in
  let domainB = Domain.spawn (fun () ->
    ignore (additem linkedlist 4 barrier);
    ignore (additem linkedlist 5 barrier);
    ignore (additem linkedlist 6 barrier);
    ignore (removeitem linkedlist 15 barrier);
    ignore (removeitem linkedlist 1 barrier)
  ) in
  Domain.join domainA;
  Domain.join domainB;
  print_list linkedlist
