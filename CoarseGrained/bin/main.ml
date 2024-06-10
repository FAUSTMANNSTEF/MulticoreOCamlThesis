
let benchmarkplain () =
  let num_sets = 3 in (* Change this to the desired number of sets *)
  let start_time = Unix.gettimeofday () in
  Approach.PlainLinkedList.test_with_n_list_operations num_sets;
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "Execution time: %.6f seconds\n" elapsed_time


let benchmarkCoarseGrained () = 
  

let () = benchmarkplain ()
