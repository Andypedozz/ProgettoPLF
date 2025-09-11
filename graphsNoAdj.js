// Funzione per ottenere i vicini di un nodo da edges
function getNeighbors(node, edges) {
    return edges.filter(([u, v]) => u === node).map(([_, v]) => v);
}

// DFS normale (senza lista di adiacenza)
function dfs(node, visited, edges, stack = null, component = null) {
    visited.add(node);
    for (const neighbor of getNeighbors(node, edges)) {
        if (!visited.has(neighbor)) {
            dfs(neighbor, visited, edges, stack, component);
        }
    }

    if (stack) stack.push(node);
    if (component) component.push(node);
}

function kosaraju(graph) {
    const { nodes, edges } = graph;

    // 1. Creo il grafo trasposto
    const edgesT = edges.map(([u, v]) => [v, u]);

    // 2. DFS sul grafo originale per ottenere l'ordine di completamento
    const visited = new Set();
    const stack = [];
    for (const n of nodes) {
        if (!visited.has(n)) {
            dfs(n, visited, edges, stack);
        }
    }

    // 3. DFS sul grafo trasposto in ordine inverso
    visited.clear();
    const sccs = {};
    let i = 0;
    while (stack.length > 0) {
        const node = stack.pop();
        if (!visited.has(node)) {
            const component = [];
            dfs(node, visited, edgesT, null, component);
            sccs[i] = component;
            i++;
        }
    }

    return sccs;
}

// Funzione per costruire il grafo delle SCC
function buildSCCGraph(originalGraph, sccs) {
    const { edges } = originalGraph;

    // Mappa nodo -> id della SCC
    const nodeToSCC = new Map();
    for (const [sccId, component] of Object.entries(sccs)) {
        for (const node of component) {
            nodeToSCC.set(node, parseInt(sccId));
        }
    }

    // I nodi del nuovo grafo sono gli ID delle SCC
    const sccNodes = Object.keys(sccs).map(id => parseInt(id));

    // Costruisco gli archi del grafo SCC
    const sccEdgesSet = new Set();
    for (const [u, v] of edges) {
        const sccU = nodeToSCC.get(u);
        const sccV = nodeToSCC.get(v);
        if (sccU !== sccV) {
            sccEdgesSet.add(`${sccU}-${sccV}`);
        }
    }

    const sccEdges = Array.from(sccEdgesSet).map(edge => {
        const [u, v] = edge.split('-').map(Number);
        return [u, v];
    });

    return {
        nodes: sccNodes,
        edges: sccEdges,
        nodeToSCC,
        sccs
    };
}

// DFS sulle SCC (usando edges invece di liste di adiacenza)
function dfsSCC(node, visited, edges) {
    visited.add(node);
    for (const neighbor of getNeighbors(node, edges)) {
        if (!visited.has(neighbor)) {
            dfsSCC(neighbor, visited, edges);
        }
    }
}

// Funzione per calcolare il numero minimo di archi da aggiungere
function calculateMinimumEdgesToAdd(startingNode, originalGraph, sccGraph) {
    const { nodes, edges, nodeToSCC } = sccGraph;

    // 1. Trovo la SCC che contiene il nodo di partenza
    const startSCC = nodeToSCC.get(startingNode);

    // 2. Trovo tutte le SCC raggiungibili da quella di partenza
    const visited = new Set();
    dfsSCC(startSCC, visited, edges);

    // 3. Calcolo il grado entrante di ogni SCC
    const indegree = new Map();
    for (const n of nodes) indegree.set(n, 0);
    for (const [u, v] of edges) {
        indegree.set(v, indegree.get(v) + 1);
    }

    // 4. Conto le SCC non raggiungibili che hanno indegree = 0
    let count = 0;
    for (const n of nodes) {
        if (!visited.has(n) && indegree.get(n) === 0) {
            count++;
        }
    }

    return count;
}

// Funzione per generare un grafo casuale
function generateRandomGraph(size) {
    const nodes = Array.from({ length: size }, (_, i) => i);
    const edges = [];

    for (let u = 0; u < size; u++) {
        const numEdges = Math.floor(Math.random() * Math.floor(size / 2)) + 1;
        for (let k = 0; k < numEdges; k++) {
            const v = Math.floor(Math.random() * size);
            if (!edges.some(([x, y]) => x === u && y === v)) {
                edges.push([u, v]);
            }
        }
    }

    return { nodes, edges };
}

function getRandomInt(size) {
    return Math.floor(Math.random() * size);
}

function main() {
    const graph = generateRandomGraph(10);
    const startingNode = getRandomInt(10);

    console.log("Grafo originale:");
    console.log("Nodi:", graph.nodes);
    console.log("Archi:", graph.edges);

    const sccs = kosaraju(graph);
    console.log("\nComponenti fortemente connesse:");
    console.log(sccs);

    const sccGraph = buildSCCGraph(graph, sccs);
    console.log("\nGrafo delle SCC:");
    console.log("Nodi:", sccGraph.nodes);
    console.log("Archi:", sccGraph.edges);

    const minEdgesToAdd = calculateMinimumEdgesToAdd(startingNode, graph, sccGraph);

    console.log(`\n=== RISULTATO FINALE ===`);
    console.log(`Numero minimo di archi da aggiungere per rendere il grafo`);
    console.log(`totalmente raggiungibile dal nodo ${startingNode}: ${minEdgesToAdd}`);
}

main();
