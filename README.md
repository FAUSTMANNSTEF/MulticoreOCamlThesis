In this project, we aim to delve into the scope of concurrent programming in OCaml. Influenced by the book The Art of Multiprocessor Programming , we will focus on 
benchmarking Coarse-Grained, Fine-Grained and Non-Blocking synchronization by implementing a linked list (PlainLinkedList.ml). We will assess these approaches in
terms of efficiency and effectiveness across multiple dimensions, such as execution
time, scalability and memory overhead. Through the insights gained from this study,
we aim to contribute to future related work on synchronization in OCaml. We expect
the project's outcome to provide valuable insights into optimizing concurrent
programmings strategies.

Ocaml Version used to compile :
 - ocaml-base-compiler.5.1.1
Opam Version used :
 - 2.0.5
Dune version used :
 - 3.15.2

To compile the code :
dune exec compare

To compile the tests :
dune runtest

To compile a specific test you compile the executable within the test file :

dune exec PathtoTestfile/"File:.exe
example:
dune exec test/test_PlainLinkedList.exe
