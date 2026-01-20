function kosaraju(graph) {
    const visited = {};
    const assigned = {};
    const L = [];

    // Costruisci lista di adiacenza
    const adj = {};
    for (let u of graph.nodes) {
        adj[u] = [];
        visited[u] = false;
        assigned[u] = null;
    }
    for (let [u, v] of graph.edges) {
        adj[u].push(v);
    }

    // Subroutine Visit
    function Visit(u) {
        if (!visited[u]) {
            visited[u] = true;
            for (let v of adj[u]) {
                Visit(v);
            }
            // Prepend u a L
            L.unshift(u);
        }
    }

    // Prima fase: visita tutti i nodi
    for (let u of graph.nodes) {
        Visit(u);
    }

    // Costruisci in-neighbours
    const inGraph = {};
    for (let u of graph.nodes) {
        inGraph[u] = [];
    }
    for (let [u, v] of graph.edges) {
        inGraph[v].push(u);
    }

    // Subroutine Assign
    function Assign(u, root) {
        if (assigned[u] === null) {
            assigned[u] = root;
            for (let v of inGraph[u]) {
                Assign(v, root);
            }
        }
    }

    // Seconda fase: assegna componenti
    for (let u of L) {
        Assign(u, u);
    }

    return assigned; // ritorna un oggetto { nodo: componenteRadice }
}

// Esempio
const graph = {
    nodes: [1,2,3,4,5,6],
    edges: [
        [1,2],
        [2,3],
        [3,1],
        [3,4],
        [4,5],
        [5,6],
        [6,4]
    ]
};

const components = kosaraju(graph);
console.log(components);
const result = {};
for (const [node, component] of Object.entries(components)) {
    result[component] = result[component] || [];
    result[component].push(parseInt(node));
}
console.log(result);