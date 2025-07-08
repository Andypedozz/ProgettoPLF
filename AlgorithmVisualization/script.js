document.getElementById('startBtn').addEventListener('click', startRandomGraph);

let network; // usato per manipolare il grafo durante l’animazione

function startRandomGraph() {
  const { nodes, edges, start } = generateRandomGraph(8, 15); // 8 nodi, 15 archi
  const graph = buildGraph(nodes, edges);

  // Crea e visualizza grafo iniziale
  drawGraph("original-graph", graph, { start });

  // Esegui animazione Kosaraju
  animateKosaraju(graph, nodes, start).then(({ scc, componentGraph }) => {
    const componentIdOfStart = findComponentOf(start, scc);
    const zeroIndegreeCount = countZeroIndegree(componentGraph, componentIdOfStart);

    drawGraph("component-graph", componentGraph, { isComponent: true });

    document.getElementById('output').innerText =
      `Componenti fortemente connesse con grado entrante 0 (esclusa quella contenente il nodo ${start}): ${zeroIndegreeCount}`;
  });
}


function buildGraph(nodes, edges) {
  const adj = {};
  nodes.forEach(n => adj[n] = []);
  edges.forEach(({ from, to }) => adj[from].push(to));
  return { nodes, edges, adj };
}

function drawGraph(containerId, graph, options = {}) {
  const { start, isComponent = false } = options;

  const nodes = graph.nodes.map(n => ({
    id: n,
    label: isComponent ? `C${n}` : `${n}`,
    color: n === start ? '#ffa500' : '#97C2FC'
  }));

  const edges = graph.edges.map(({ from, to }) => ({
    from, to, arrows: 'to'
  }));

  const container = document.getElementById(containerId);
  const data = {
    nodes: new vis.DataSet(nodes),
    edges: new vis.DataSet(edges)
  };

  const visOptions = {
    nodes: { shape: "circle" },
    physics: false
  };

  if (containerId === "original-graph") {
    network = new vis.Network(container, data, visOptions);
  } else {
    new vis.Network(container, data, visOptions);
  }
}

// Kosaraju animato
async function animateKosaraju(graph, nodes, start) {
  const visited = new Set();
  const order = [];
  const adj = graph.adj;

  // DFS normale (registrazione ordine di uscita)
  async function dfs(u) {
    visited.add(u);
    highlightNode(u, "#add8e6");
    await wait(500);
    for (const v of adj[u]) {
      if (!visited.has(v)) {
        await dfs(v);
      }
    }
    order.push(u);
    highlightNode(u, "#90ee90");
    await wait(300);
  }

  for (const n of nodes) {
    if (!visited.has(n)) await dfs(n);
  }

  const transpose = {};
  nodes.forEach(n => transpose[n] = []);
  for (const [u, list] of Object.entries(adj)) {
    list.forEach(v => transpose[v].push(parseInt(u)));
  }

  visited.clear();
  const scc = [];

  async function dfsRev(u, component, color) {
    visited.add(u);
    component.push(u);
    highlightNode(u, color);
    await wait(300);
    for (const v of transpose[u]) {
      if (!visited.has(v)) {
        await dfsRev(v, component, color);
      }
    }
  }

  for (let i = order.length - 1; i >= 0; i--) {
    const u = order[i];
    if (!visited.has(u)) {
      const component = [];
      const randomColor = getRandomColor();
      await dfsRev(u, component, randomColor);
      scc.push(component);
    }
  }

  const componentGraph = buildComponentGraph(graph, scc);
  return { scc, componentGraph };
}

function buildComponentGraph(graph, scc) {
  const nodeToComponent = {};
  scc.forEach((component, idx) => {
    component.forEach(node => nodeToComponent[node] = idx);
  });

  const compAdj = {};
  scc.forEach((_, idx) => compAdj[idx] = new Set());

  for (const { from, to } of graph.edges) {
    const fromComp = nodeToComponent[from];
    const toComp = nodeToComponent[to];
    if (fromComp !== toComp) {
      compAdj[fromComp].add(toComp);
    }
  }

  const nodes = Object.keys(compAdj).map(k => parseInt(k));
  const edges = [];
  for (const [u, targets] of Object.entries(compAdj)) {
    for (const v of targets) {
      edges.push({ from: parseInt(u), to: v });
    }
  }

  return { nodes, edges, adj: compAdj };
}

function findComponentOf(start, scc) {
  for (let i = 0; i < scc.length; i++) {
    if (scc[i].includes(start)) return i;
  }
  return -1;
}

function countZeroIndegree(componentGraph, excludeId) {
  const indegree = {};
  componentGraph.nodes.forEach(n => indegree[n] = 0);
  componentGraph.edges.forEach(({ to }) => indegree[to]++);

  return componentGraph.nodes.filter(n => n !== excludeId && indegree[n] === 0).length;
}

function highlightNode(id, color) {
  const node = network.body.data.nodes.get(id);
  if (node) {
    network.body.data.nodes.update({ id, color });
  }
}

function wait(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

function getRandomColor() {
  const letters = '89ABCDEF';
  return '#' + Array.from({ length: 6 }, () => letters[Math.floor(Math.random() * letters.length)]).join('');
}

function generateRandomGraph(nodeCount, edgeCount) {
  const nodes = Array.from({ length: nodeCount }, (_, i) => i + 1);
  const edges = new Set();

  while (edges.size < edgeCount) {
    const from = nodes[Math.floor(Math.random() * nodeCount)];
    const to = nodes[Math.floor(Math.random() * nodeCount)];
    if (from !== to) {
      edges.add(`${from},${to}`);
    }
  }

  const edgeList = Array.from(edges).map(e => {
    const [from, to] = e.split(',').map(Number);
    return { from, to };
  });

  const start = nodes[Math.floor(Math.random() * nodeCount)];
  return { nodes, edges: edgeList, start };
}
