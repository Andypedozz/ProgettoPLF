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

function printGraphs(graphs) {
    graphs.forEach(graph => {
        const { vs, as } = graph;
        vs.forEach(v => console.log(v))

        as.forEach(a => {
            console.log(`${a[0]} ${a[1]}`)
        })
    });
}

function main() {
    let graphs = [
        // Ciclo semplice, nodo di partenza 1
        { vs: [1, 2, 3, 4, 5], as: [[1, 2], [2, 3], [3, 4], [4, 5], [5, 1]], startingNode: 1 },

        // Ciclo con nodo isolato, partenza 3
        { vs: [1, 2, 3, 4, 5, 6], as: [[1, 2], [2, 3], [3, 1], [4, 5]], startingNode: 3 },

        // Grafo diretto con biforcazioni, partenza 1
        { vs: [1, 2, 3, 4, 5, 6], as: [[1, 2], [1, 3], [2, 4], [3, 4], [4, 5], [4, 6]], startingNode: 1 },

        // Cicli interconnessi, partenza 2
        { vs: [1, 2, 3, 4, 5, 6, 7], as: [[1, 2], [2, 3], [3, 1], [3, 4], [4, 5], [5, 3], [5, 6], [6, 7]], startingNode: 2 },

        // Grafo con loop singolo e nodo isolato, partenza 5
        { vs: [1, 2, 3, 4, 5, 6], as: [[1, 2], [2, 3], [3, 3], [4, 5]], startingNode: 5 },

        // Grafo a stella direzionale, partenza 1
        { vs: [1, 2, 3, 4, 5, 6], as: [[1, 2], [1, 3], [1, 4], [1, 5], [1, 6]], startingNode: 1 },

        // Grafi con cicli multipli e componenti connesse separate, partenza 4
        { vs: [1, 2, 3, 4, 5, 6, 7, 8], as: [[1, 2], [2, 3], [3, 1], [4, 5], [5, 6], [6, 4], [6, 7], [7, 8]], startingNode: 4 },

        // Grafo lineare lungo con loop finale, partenza 1
        { vs: [1, 2, 3, 4, 5, 6, 7], as: [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 7], [7, 4]], startingNode: 1 },

        // Grafo complesso con cicli nidificati, partenza 2
        { vs: [1, 2, 3, 4, 5, 6, 7, 8], as: [[1, 2], [2, 3], [3, 1], [3, 4], [4, 5], [5, 3], [5, 6], [6, 7], [7, 6], [7, 8]], startingNode: 2 },

        // Grafo con percorsi paralleli e loop, partenza 1
        { vs: [1, 2, 3, 4, 5, 6, 7], as: [[1, 2], [2, 3], [3, 1], [2, 4], [4, 5], [5, 6], [6, 4], [6, 7]], startingNode: 1 }
    ];

    let i = 1;
    graphs.forEach(graph => {
        const startingNode = graph.startingNode;
        console.log("\n##################################################################")
        console.log("Test "+i);
        console.log("Grafo originale:");
        console.log("Nodi:", graph.vs);
        console.log("Archi:", graph.as);

        const sccs = kosaraju({ nodes: graph.vs, edges: graph.as });
        console.log("\nComponenti fortemente connesse:");
        console.log(sccs);

        const sccGraph = buildSCCGraph({ nodes: graph.vs, edges: graph.as }, sccs);
        console.log("\nGrafo delle SCC:");
        console.log("Nodi:", sccGraph.nodes);
        console.log("Archi:", sccGraph.edges);

        const minEdgesToAdd = calculateMinimumEdgesToAdd(startingNode, { nodes: graph.vs, edges: graph.as }, sccGraph);

        console.log(`\n=== RISULTATO FINALE ===`);
        console.log(`Numero minimo di archi da aggiungere per rendere il grafo`);
        console.log(`totalmente raggiungibile dal nodo ${startingNode}: ${minEdgesToAdd}`);
        console.log("##################################################################\n")
        i++;
    })
}

main();
