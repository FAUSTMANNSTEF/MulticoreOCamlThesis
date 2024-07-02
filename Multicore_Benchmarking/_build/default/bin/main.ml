(* Function to read an integer from user input *)
let read_int_with_prompt prompt =
  print_string prompt;
  read_int ()

(* Function to read a float from user input *)
let read_float_with_prompt prompt =
  print_string prompt;
  read_float ()

(* Function to read a boolean from user input *)
let read_bool_with_prompt prompt =
  print_string prompt;
  let response = read_line () in
  response = "yes" || response = "y"

(* Function to read a list of integers from user input *)
let read_int_list_with_prompt prompt =
  print_string prompt;
  let line = read_line () in
  List.map int_of_string (String.split_on_char ' ' line)

(* Randomly generate elements *)
let generate_random_list n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n - 1) (Random.int 10000 :: acc)
  in
  aux n []

(* Add elements to the linked list *)
let add_elements linkedlist elements additem_fn =
  List.iter (fun el -> ignore (additem_fn linkedlist el)) elements

(* Function to generate random operations based on the given ratios *)
let generate_operations num_ops add_ratio del_ratio =
  let rec aux n acc =
    if n = 0 then acc
    else
      let op =
        let r = Random.float 1.0 in
        if r < add_ratio then 3 (* insert *)
        else if r < add_ratio +. del_ratio then 2 (* delete *)
        else 0 (* find/contains *)
      in
      aux (n - 1) (op :: acc)
  in
  aux num_ops []

(* Benchmark function *)
let benchmark create_linkedlist_fn additem_fn benchmark_fn num_domains random_list operations_list =
  let linkedlist = create_linkedlist_fn () in
  add_elements linkedlist random_list additem_fn;
  let start_time = Unix.gettimeofday () in
  benchmark_fn num_domains linkedlist operations_list;
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  elapsed_time

(* Benchmark setup for different approaches *)
let do_benchmark num_elements num_list_operations num_iters num_domains add_ratio del_ratio  =
  let random_list = generate_random_list num_elements in
  let operations_list = List.init num_domains (fun _ -> generate_operations (num_list_operations / num_domains) add_ratio del_ratio ) in

  let make_bm create_fn add_fn bench_fn () =
    let f = fun _ -> (benchmark create_fn add_fn bench_fn num_domains random_list operations_list) in
    let lst = List.init num_iters f in
    let s = List.fold_left (fun acc a -> acc +. a) 0.0 lst in
    s /. (float_of_int num_iters)
  in

  (* Coarse-Grained Linked List Benchmark *)
  let coarse_bm = make_bm
                    Approach.CoarseGrained.create_linkedlist
                    Approach.CoarseGrained.additem
                    Approach.CoarseGrained.benchmark () in

  (* Fine-Grained Linked List Benchmark *)
  let fine_bm = make_bm
                  Approach.FineGrained.create_linkedlist
                  Approach.FineGrained.additem
                  Approach.FineGrained.benchmark () in

  (* Lock-Free Linked List Benchmark *)
  let lockfree_bm = make_bm
                      Approach.LockFree.create_linkedlist
                      Approach.LockFree.additem
                      Approach.LockFree.benchmark () in

  (* Print Results *)
  List.iter (fun (name, bm_avg) ->
    Printf.printf "Benchmark for %s (%i domains): %.6f\n" name num_domains bm_avg
  ) [
    ("CoarseGrained", coarse_bm);
    ("FineGrained", fine_bm);
    ("LockFree", lockfree_bm)
  ];
  print_newline ()


let rec main_loop () =
  let num_elements = read_int_with_prompt "Enter the number of elements: " in
  let num_list_operations = read_int_with_prompt "Enter the number of list operations: " in
  let num_iters = read_int_with_prompt "Enter the number of iterations: " in

  let add_ratio = read_float_with_prompt "Enter the ratio of additions (0.0 - 1.0): " in
  let del_ratio = read_float_with_prompt "Enter the ratio of deletions (0.0 - 1.0): " in
  let cont_ratio = read_float_with_prompt "Enter the ratio of contains (0.0 - 1.0): " in

  (* Ensure the ratios sum up to 1.0 *)
  if add_ratio +. del_ratio +. cont_ratio <> 1.0 then
    failwith "The sum of the ratios must be 1.0";

  let multiple_domains = read_bool_with_prompt "Do you want to test a sequence of domains? (yes/no): " in

  if multiple_domains then
    let domain_sequence = read_int_list_with_prompt "Enter the sequence of domains (space-separated): " in
    List.iter (fun num_domains ->
      do_benchmark num_elements num_list_operations num_iters num_domains add_ratio del_ratio
    ) domain_sequence
  else
    let num_domains = read_int_with_prompt "Enter the number of domains: " in
    do_benchmark num_elements num_list_operations num_iters num_domains add_ratio del_ratio;
    
    
  let continue = read_bool_with_prompt "Do you want to run another benchmark? (yes/no): " in
  if continue then main_loop ()
  else print_endline "Benchmarking completed."
  
let () = main_loop ()