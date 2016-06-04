The architecture of this tool is described in Section 3 of our paper.

To run our examples, please see toy.rkt. This file contains (1) sample programs and (2) imports sample graphs from examples.rkt. Running toy.rkt will execute the program "programFoodWeb-hard" on the dataset "food-web" as in Figure 3 of the paper.

json->graph converts a JSON graph to the format used by SymVis.
translate takes as input a graph and program to complete the translation phase.
(solve (asserts)) runs the solver.
graph->json takes as input a graph, state, and assignments and produces the cola.js constraints in JSON format.