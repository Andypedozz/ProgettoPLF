// Funzione per inserire i dati del grafo in una lista (map) di adiacenza
function buildAdjList(nodes, edges) {
    const adj = new Map();
    for(const n of nodes) adj.set(n, []);
    for(const [u, v] of edges) {
        adj.get(u).push(v);
    }
    return adj;
}

// DFS normale
function dfs(node, visited, adj, stack = null, component = null) {
    visited.add(node);
    for(const neighbor of adj.get(node)) {
        if(!visited.has(neighbor)) {
            dfs(neighbor, visited, adj, stack, component);
        }
    }

    if(stack) stack.push(node);
    if(component) component.push(node);
}

function kosaraju(graph) {
    const { nodes, edges} = graph;

    // 1. Costruisco il grafo e il suo trasposto
    const adj = buildAdjList(nodes, edges);
    const adjT = buildAdjList(nodes, edges.map(([u, v]) => [v, u]));

    // 2. DFS sul grafo originare per ottenere l'ordine di completamento
    const visited = new Set();
    const stack = [];
    for(const n of nodes) {
        if(!visited.has(n)) {
            dfs(n, visited, adj, stack);
        }
    }

    // 3. DFS sul grafo trasposto in ordine inverso
    visited.clear();
    const sccs = {}
    let i = 0;
    while(stack.length > 0) {
        const node = stack.pop();
        if(!visited.has(node)) {
            const component = []
            dfs(node, visited, adjT, null, component);
            sccs[i] = component;
            i++;
        }
    }

    return sccs;
}

// Funzione per costruire il grafo delle componenti fortemente connesse
function buildSCCGraph(originalGraph, sccs) {
    const { nodes, edges } = originalGraph;
    
    // Creo una mappa che associa ogni nodo originale alla sua componente SCC
    const nodeToSCC = new Map();
    for(const [sccId, component] of Object.entries(sccs)) {
        for(const node of component) {
            nodeToSCC.set(node, parseInt(sccId));
        }
    }
    
    // I nodi del nuovo grafo sono gli ID delle SCC
    const sccNodes = Object.keys(sccs).map(id => parseInt(id));
    
    // Costruisco gli archi del grafo SCC
    const sccEdgesSet = new Set(); // Uso un Set per evitare duplicati
    
    for(const [u, v] of edges) {
        const sccU = nodeToSCC.get(u);
        const sccV = nodeToSCC.get(v);
        
        // Aggiungo un arco solo se collega componenti diverse
        if(sccU !== sccV) {
            sccEdgesSet.add(`${sccU}-${sccV}`);
        }
    }
    
    // Converto il Set in array di coppie
    const sccEdges = Array.from(sccEdgesSet).map(edge => {
        const [u, v] = edge.split('-').map(id => parseInt(id));
        return [u, v];
    });
    
    // Costruisco la lista di adiacenza per il grafo SCC
    const sccAdjList = buildAdjList(sccNodes, sccEdges);
    
    return {
        nodes: sccNodes,
        edges: sccEdges,
        adjList: sccAdjList,
        nodeToSCC: nodeToSCC, // Mappa per tracciare quale nodo appartiene a quale SCC
        sccs: sccs // Manteniamo anche le componenti originali
    };
}

// Funzione per calcolare il numero minimo di archi da aggiungere
function calculateMinimumEdgesToAdd(startingNode, originalGraph, sccGraph) {
    const { nodes, edges, adjList, nodeToSCC } = sccGraph;

    // 1. Trovo la SCC che contiene il nodo di partenza
    const startSCC = nodeToSCC.get(startingNode);

    // 2. Trovo tutte le SCC raggiungibili da quella di partenza
    const visited = new Set();
    function dfsSCC(node) {
        visited.add(node);
        for(const neighbor of adjList.get(node)) {
            if(!visited.has(neighbor)) {
                dfsSCC(neighbor);
            }
        }
    }
    dfsSCC(startSCC);

    // 3. Calcolo il grado entrante di ogni SCC
    const indegree = new Map();
    for(const n of nodes) indegree.set(n, 0);
    for(const [u, v] of edges) {
        indegree.set(v, indegree.get(v) + 1);
    }

    // 4. Conto le SCC NON raggiungibili che hanno indegree = 0
    let count = 0;
    for(const n of nodes) {
        if(!visited.has(n) && indegree.get(n) === 0) {
            count++;
        }
    }

    return count;
}

// Funzione per generare un grafo casuale
function generateRandomGraph(size) {
    const nodes = Array.from({ length: size }, (_, i) => i);
    const edges = [];

    // Ogni nodo deve avere almeno un arco uscente
    for (let u = 0; u < size; u++) {
        // Numero casuale di archi uscenti da u (almeno 1, massimo size/2 per non esagerare)
        const numEdges = Math.floor(Math.random() * (Math.floor(size / 2))) + 1;

        for (let k = 0; k < numEdges; k++) {
            const v = Math.floor(Math.random() * size);

            // Evitiamo di inserire più volte lo stesso arco
            const edgeStr = `${u}-${v}`;
            if (!edges.some(([x, y]) => x === u && y === v)) {
                edges.push([u, v]);
            }
        }
    }

    return { nodes, edges };
}

function getRandomInt(size) {
    return Math.floor(Math.random() * (size + 1));
}


function main() {
    const graph = generateRandomGraph(10);
    const startingNode = getRandomInt(10); // <--- puoi cambiare il nodo di partenza
    
    const map = buildAdjList(graph.nodes, graph.edges);
    console.log("Grafo originale:");
    console.log(map);
    
    const sccs = kosaraju(graph);
    console.log("\nComponenti fortemente connesse:");
    console.log(sccs);
    
    // Costruisco il grafo delle SCC
    const sccGraph = buildSCCGraph(graph, sccs);
    console.log("\nGrafo delle SCC:");
    console.log("Nodi:", sccGraph.nodes);
    console.log("Archi:", sccGraph.edges);
    console.log("Lista di adiacenza:");
    console.log(sccGraph.adjList);
    
    // Calcolo il numero minimo di archi da aggiungere
    const minEdgesToAdd = calculateMinimumEdgesToAdd(startingNode, graph, sccGraph);
    
    console.log(`\n=== RISULTATO FINALE ===`);
    console.log(`Numero minimo di archi da aggiungere per rendere il grafo`);
    console.log(`totalmente raggiungibile dal nodo ${startingNode}: ${minEdgesToAdd}`);
}

main();
