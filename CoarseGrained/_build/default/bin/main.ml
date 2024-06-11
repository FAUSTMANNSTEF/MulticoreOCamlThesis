let num_domains = 2
let num_list_operations = 5000
let num_elements = 3000 

(* Randomly generate elements *)
let generate_random_list n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n - 1) (Random.int 10000 :: acc)
  in
  aux n []

let benchmarkplain num_domains num_elements num_list_operations =
  let random_list = generate_random_list num_elements in
  let start_time = Unix.gettimeofday () in
  Approach.PlainLinkedList.benchmark num_domains random_list num_list_operations;
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "Execution time of Plain linked list: %.6f seconds\n" elapsed_time

let benchmarkCoarseGrained num_domains num_elements num_list_operations =
  let random_list = generate_random_list num_elements in
  let start_time = Unix.gettimeofday () in
  Approach.CoarseGrained.benchmark num_domains random_list num_list_operations;
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "Execution time of CoarseGrained linked list: %.6f seconds\n" elapsed_time

let benchmarkFineGrained num_domains num_elements num_list_operations =
  let random_list = generate_random_list num_elements in
  let start_time = Unix.gettimeofday () in
  Approach.FineGrained.benchmark num_domains random_list num_list_operations;
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "Execution time of FineGrained linked list: %.6f seconds\n" elapsed_time

let () =
  benchmarkplain num_domains num_elements num_list_operations;
  benchmarkFineGrained num_domains num_elements num_list_operations;
  benchmarkCoarseGrained num_domains num_elements num_list_operations