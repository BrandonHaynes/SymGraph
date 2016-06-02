function makeTree(numberOfNodes) {
  var nodes = [{"id": 0}];
  var links = [];
  for (var i = 1; i < numberOfNodes; i++) {
    links.push({"source": Math.floor(Math.random() * nodes.length), "target": i})
    nodes.push({"id": i});
  };
  return {"nodes": nodes, "links": links};
};

function makeClassTree(numberOfNodes, classes) {
  var nodes = [{"id": 0, "type": classes[Math.floor(Math.random() * classes.length)]}];
  var links = [];
  for (var i = 1; i < numberOfNodes; i++) {
    links.push({"source": Math.floor(Math.random() * nodes.length), "target": i})
    nodes.push({"id": i, "type": classes[Math.floor(Math.random() * classes.length)});
  };
  return {"nodes": nodes, "links": links};
}

function makeGraph(numberOfNodes, numberOfEdges) {
  var tree = makeClassTree(numberOfNodes, ["carnivore", "omnivore", "herbivore"]);
  var nodes = tree.nodes;
  var links = tree.links;

  if(numberOfEdges < numberOfNodes - 1) numberOfEdges = numberOfNodes;
  while(links.length < numberOfEdges) {
    var source = Math.floor(Math.random() * nodes.length);
    var target = Math.floor(Math.random() * nodes.length);
    var edge = {"source": source, "target": target};
    var reverse = {"source": target, "target": source};
    if(source != target && links.indexOf(edge) == -1 && links.indexOf(reverse) == -1) links.push(edge);
  }
};