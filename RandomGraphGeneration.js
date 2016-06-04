function makeTree(numberOfNodes) {
  var nodes = [{"id": 0}];
  var links = [];
  for (var i = 1; i < numberOfNodes; i++) {
    links.push({"source": Math.floor(Math.random() * nodes.length), "target": i})
    nodes.push({"id": i});
  };
  return {"nodes": nodes, "links": links};
};

function makeClassTree(numberOfNodes, classes, offset) {
  var nodes = [{"id": 0, "type": classes[Math.floor(Math.random() * classes.length)]}];
  var links = [];
  for (var i = 1; i < numberOfNodes; i++) {
    links.push({"source": Math.floor(Math.random() * nodes.length) + offset, "target": i + offset})
    nodes.push({"id": i + offset, "type": classes[Math.floor(Math.random() * classes.length)]});
  };
  return {"nodes": nodes, "links": links};
}

function makeGraph(numberOfNodes, numberOfEdges, type, offset) {
  var tree = makeClassTree(numberOfNodes, [type], offset);
  var nodes = tree.nodes;
  var links = tree.links;

  if(numberOfEdges < numberOfNodes - 1) numberOfEdges = numberOfNodes;
  while(links.length < numberOfEdges) {
    var source = Math.floor(Math.random() * nodes.length) + offset;
    var target = Math.floor(Math.random() * nodes.length) + offset;
    var edge = {"source": source, "target": target};
    var reverse = {"source": target, "target": source};
    if(source != target && links.indexOf(edge) == -1 && links.indexOf(reverse) == -1) links.push(edge);
  }
  return {"nodes": nodes, "links": links};
};

var g1 = makeGraph(15, 30, "carnivore", 0)
var g2 = makeGraph(15, 45, "herbivore", 15)
var g3 = makeGraph(15, 14, "plant", 30)

var graph = {};
graph["nodes"] = g1.nodes.concat(g2.nodes).concat(g3.nodes);

var interedges = [];
for (var i = 0; i < 15; i++) {
  var edge = {"source": Math.floor(Math.random() * g2.nodes.length) + 15, "target": Math.floor(Math.random() * g1.nodes.length)};
  if(interedges.indexOf(edge) == -1) interedges.push(edge);
};
for (var i = 0; i < g3.nodes.length; i++) {
  interedges.push({"source": i + 30, "target": Math.floor(Math.random() * g2.nodes.length) + 15})
};
for (var i = 0; i < 30; i++) {
  var edge = {"source": Math.floor(Math.random() * g3.nodes.length) + 30, "target": Math.floor(Math.random() * g2.nodes.length) + 15};
  if(interedges.indexOf(edge) == -1) interedges.push(edge);
};

graph["links"] = g1.links.concat(g2.links).concat(interedges);
