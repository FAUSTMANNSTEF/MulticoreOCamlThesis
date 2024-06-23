### Introduction (2 minutes)

- **Supervisors**:

  - Acknowledge supervisors

- **Topic Introduction**:
  - Short introduction on ocaml
  - Introduce the topic of concurrent programming using Multicore OCaml.
  - Mention the recent addition of Multicore OCaml and mention that there is limited existing usage and exploration.
  - Mention inspiration from other papers "The Art of Multiprocessor Programming" and benchmark studies in Haskell paper.

### Main Part

- **Research Focus**:

  - Objective: Compare different synchronization approaches in concurrent ocaml

- **Linked List Operations**:

  - mention linked list data structure and the operations we aim to paralelize ( explain each operation a bit)

- **Synchronization Approaches**:

  - Mention the synchronization approaches (e.g., Fine grained, lock-free etc.) .
  - Briefly outline their differences in terms of concurrency control and performance characteristics.
    Z

- **Benchmarking Approach**:

  - Describe our methodology for benchmarking:
    - How we approached performance testing and measurement. ( also mention metrics)
    - Briefly present significant results or trends observed (consider showing a relevant graph or chart).

- **Individual Testing of Approaches**:
  - Discuss any insights gained from individual testing of synchronization approaches.
  - Mention Testing that was done to each file individually

### Q&A (2 minutes)

- **Engage with the Audience**:

##### Dan questions

- Remove figures from all of the pictures in the slides
- Dan ask about synchronization approaches

- Ask about graphs and stuff to add
