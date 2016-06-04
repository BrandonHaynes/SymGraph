The architecture of this tool is described in Section 3 of our paper.

This code requires Rosette to run, which can be found at https://emina.github.io/rosette/.

To run all of our benchmarks, see benchmarks.rkt. Running this will produce the resulting JSON for all of the examples shown in the paper.

- examples.rkt contains a variety of example graphs used in the benchmarks and paper.
- programs.rkt contains a variety of sample programs used in the benchmarks and paper.

Alternatively, to run a single example, please see toy.rkt. This file (1) imports sample programs from programs.rkt and (2) imports sample graphs from examples.rkt. Running toy.rkt will execute the program "programFoodWeb-hard" on the dataset "food-web" as in Figure 3 of the paper.

- json->graph converts a JSON graph to the format used by SymVis.
- translate takes as input a graph and program and executes SymVis.
- (solve (asserts)) runs the solver.
- graph->json takes as input a graph, state, and assignments and produces the cola.js constraints in JSON format.

Once you have produced a graph in JSON format, you can display it by running “python -m SimpleHTTPServer 9000” in the containing folder for this code and accessing http://localhost:9000/SlashName.html. On this page, you can replace the graph in the text box and press “Submit” to view the result.

