/**
 * 1. Algoritmo di Kosaraju
 *  - DFS sul grafo originale per ottenere l'ordine di completamento
 *  - DFS sul grafo trasposto in ordine inverso per ottenere le SCC
 * 2. Ricostruzione del grafo compresso
 * 3. Conteggio SCC (eventuale)
 */

const graph = {
    nodes: [0,1,2,3,4,5,6,7],
    edges: [
        [0,1],
        [1,2],
        [2,1],
        [2,3],
        [3,4],
        [4,5],
        [5,6],
        [6,4],
        [6,7]
    ]
};

function kosaraju(graph) {
    const postOrder = postOrderDfs(graph);
    const invertedGraph = invertGraph(graph);
    const visited = new Set();
    const sccs = [];

    for (const node of postOrder) {
        if (!visited.has(node)) {
            const component = [];
            dfs(node, visited, invertedGraph.edges, component);
            sccs.push(component);
        }
    }

    return sccs;
}

function postOrderDfs(graph) {
    const visited = new Set();
    const postOrder = [];

    for (const node of graph.nodes) {
        if (!visited.has(node)) {
            dfs(node, visited, graph.edges, postOrder);
        }
    }

    return postOrder.reverse(); // necessario per Kosaraju
}

function dfs(node, visited, edges, result) {
    visited.add(node);

    for (const neighbor of getNeighbors(node, edges)) {
        if (!visited.has(neighbor)) {
            dfs(neighbor, visited, edges, result);
        }
    }

    result.push(node);
}

function getNeighbors(node, edges) {
    return edges
        .filter(([u]) => u === node)
        .map(([_, v]) => v);
}

function invertGraph(graph) {
    const invertedEdges = graph.edges.map(([u, v]) => [v, u]);
    return {
        nodes: graph.nodes,
        edges: invertedEdges
    };
}

function createCompressedGraph(sccs, graph) {
    const compressedGraph = { nodes: [], edges: [] };
    const sccMap = new Map();

    // Mappa nodo → rappresentante SCC
    for (const scc of sccs) {
        const rep = scc[0];
        compressedGraph.nodes.push(rep);
        for (const node of scc) {
            sccMap.set(node, rep);
        }
    }

    // Aggiunta archi tra SCC
    for (const [u, v] of graph.edges) {
        const su = sccMap.get(u);
        const sv = sccMap.get(v);
        if (su !== sv) {
            compressedGraph.edges.push([su, sv]);
        }
    }

    return compressedGraph;
}

function main() {
    const sccs = kosaraju(graph);
    const compressedGraph = createCompressedGraph(sccs, graph);
    console.log("SCC:", sccs);
    console.log("Grafo compresso:", compressedGraph);
}

main();
