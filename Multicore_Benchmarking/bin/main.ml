let num_domains = 3
let num_list_operations = 1000
let num_elements = 1000

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

(* Benchmark setup for different approaches *)
let () =
  let random_list = generate_random_list num_elements in
  let operations_list = List.init num_domains (fun _ -> generate_operations num_list_operations) in

  (* Helper function to wrap the benchmark call *)
  let wrap_benchmark name create_fn add_fn bench_fn =
    fun () -> benchmark name create_fn add_fn bench_fn num_domains random_list operations_list
  in

  (* Plain Linked List Benchmark *)
  let plain_bm = Benchmark.latency1 4L (wrap_benchmark "Plain" 
                                        Approach.PlainLinkedList.create_linkedlist 
                                        Approach.PlainLinkedList.additem 
                                        Approach.PlainLinkedList.benchmark) () in
  
  (* Coarse-Grained Linked List Benchmark *)
  let coarse_bm = Benchmark.latency1 4L (wrap_benchmark "CoarseGrained" 
                                         Approach.CoarseGrained.create_linkedlist 
                                         Approach.CoarseGrained.additem 
                                         Approach.CoarseGrained.benchmark) () in
  
  (* Fine-Grained Linked List Benchmark *)
  let fine_bm = Benchmark.latency1 4L (wrap_benchmark "FineGrained" 
                                       Approach.FineGrained.create_linkedlist 
                                       Approach.FineGrained.additem 
                                       Approach.FineGrained.benchmark) () in
  
  (* Lock-Free Linked List Benchmark *)
  let lockfree_bm = Benchmark.latency1 4L (wrap_benchmark "LockFree" 
                                           Approach.LockFree.create_linkedlist 
                                           Approach.LockFree.additem 
                                           Approach.LockFree.benchmark) () in

  (* Print Results *)
  List.iter (fun (name, bm_list) ->
    Printf.printf "Benchmark for %s:\n" name;
    List.iter (fun bm ->
      Printf.printf "%s\n" (Benchmark.to_string ~style:All bm)
    ) bm_list
  ) [
    ("Plain", snd (List.hd plain_bm));
    ("CoarseGrained", snd (List.hd coarse_bm));
    ("FineGrained", snd (List.hd fine_bm));
    ("LockFree", snd (List.hd lockfree_bm))
  ]
(* Benchmark function
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
  benchmark "LockFree" Approach.LockFree.create_linkedlist Approach.LockFree.additem Approach.LockFree.benchmark num_domains random_list operations_list *)
