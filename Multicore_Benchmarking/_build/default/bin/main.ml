let num_domains = 3
let num_list_operations = 10000
let num_elements = 3000 

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

(* Function to generate random operations *)
let generate_operations num_ops =
  let rec aux n acc =
    if n = 0 then acc
    else
      let op = Random.int 7 in (* 0-1: find, 2: delete, 3-6: insert *)
      aux (n - 1) (op :: acc)
  in
  aux num_ops []

(* Benchmark function *)
let benchmark approach_name create_linkedlist_fn additem_fn benchmark_fn num_domains random_list operations_list =
  let linkedlist = create_linkedlist_fn () in
  add_elements linkedlist random_list additem_fn;
  let start_time = Unix.gettimeofday () in
  benchmark_fn num_domains linkedlist operations_list;
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "Execution time of %s linked list: %.6f seconds\n" approach_name elapsed_time

let () =
  let random_list = generate_random_list num_elements in
  let operations_list = List.init num_domains (fun _ -> generate_operations num_list_operations) in
  benchmark "Plain" Approach.PlainLinkedList.create_linkedlist Approach.PlainLinkedList.additem Approach.PlainLinkedList.benchmark num_domains random_list operations_list;
  benchmark "CoarseGrained" Approach.CoarseGrained.create_linkedlist Approach.CoarseGrained.additem Approach.CoarseGrained.benchmark num_domains random_list operations_list;
  benchmark "FineGrained" Approach.FineGrained.create_linkedlist Approach.FineGrained.additem Approach.FineGrained.benchmark num_domains random_list operations_list;
  benchmark "LockFree" Approach.LockFree.create_linkedlist Approach.LockFree.additem Approach.LockFree.benchmark num_domains random_list operations_list
