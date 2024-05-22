# OCaml Notes for Me

## Functions

- Function definitions at top and bottom you call them
- if you have recursion put rec in the function declaration

![alt text](image.png)

1. in basically mentions that it is in the same scope as the function itself , without the in it would state that its a syntax error
2. let declares a variable under the function name

### Pattern Matching

![alt text](image-1.png)

- match checks variable and outputs res based on input
- "\_" symbol is a wildcat and is used for else kindoff

![alt text](image-3.png)

- h= head of list and t=tail h::t

## Compile

- ocamlc -o plainlinkedlist plainlinkedlist.ml
  ./plainlinkedlist or ocamlrun
- ocamlopt -o plainlinkedlist plainlink
  edlist.ml
  ./plainlinkedlist

## Variables

- let .. assigns variable
  if i do apoel = 6 it checks it doesnt assign it (bool)
- list called x with integers
  ![alt text](image-2.png)
  1::[2;3] appends 1 to the list

#### Dune

![alt text](image-4.png)

#### Art of Multiprocessor Programming notes

- Remove (x) does and returns true if was there
- Add (x) does and returns true if not there
- Contains (x) true if set containse

Node :
![alt text](image-5.png)
2 types of nodes:

- Nodes from image
- Sentinel Nodes (1st and last),cant be removed keys minimun and maximum values

![alt text](image-6.png)

- Each thread has 2 local variables to traverese , curr and pred

- Nodes are sorted by key and keys are unique

Points to mention in my thesis:

- Explain each approach /their main differences
- Mention linearization point for each

utop Path:
#use "/mnt/d/Groningen/Year 3/Thesis/Coding/MulticoreOCamlThesis/CoarseGrained/bin/LinkedList.ml";;

Latest Notes of meeting with Dan :

- Add Read.me mentioning specifications and ocaml version etc etc
- Change the barrier so it doesnt do it every iteration maybe just in the beggingin
- implement mli files for each
- implement a generic testing
- check out saturn bennchmarking
- implement lock free synchronization
- refactor
