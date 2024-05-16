  (* Test function to initialize and manipulate the linked list *)
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
    print_list linkedlist.firstnode  (* Print the list *)