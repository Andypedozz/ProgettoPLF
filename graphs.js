// ===================
// Utility di grafo
// ===================

class Graph {
    constructor(size) {
        this.size = size;
        this.adjList = new Map();
        for (let i = 0; i < size; i++) {
            this.adjList.set(i, []);
        }
    }

    addEdge(from, to) {
        if (!this.adjList.get(from).includes(to)) {
            this.adjList.get(from).push(to);
        }
    }

    getNeighbors(node) {
        return this.adjList.get(node);
    }

    print() {
        console.log("Grafo:");
        for (let [node, neighbors] of this.adjList.entries()) {
            console.log(`${node} -> ${neighbors.join(", ")}`);
        }
    }
}

// ===================
// Genera grafo casuale
// ===================

function generateRandomGraph(nNodes, edgeProbability = 0.3) {
    const graph = new Graph(nNodes);
    for (let from = 0; from < nNodes; from++) {
        for (let to = 0; to < nNodes; to++) {
            if (from !== to && Math.random() < edgeProbability) {
                graph.addEdge(from, to);
            }
        }
    }
    return graph;
}

// ===================
// DFS per raggiungibilità
// ===================

function dfs(graph, start, visited) {
    visited[start] = true;
    for (let neighbor of graph.getNeighbors(start)) {
        if (!visited[neighbor]) {
            dfs(graph, neighbor, visited);
        }
    }
}

// ===================
// Trova nodi non raggiungibili
// ===================

function findUnreachableNodes(graph, start) {
    const visited = new Array(graph.size).fill(false);
    dfs(graph, start, visited);
    const unreachable = [];
    for (let i = 0; i < graph.size; i++) {
        if (!visited[i]) {
            unreachable.push(i);
        }
    }
    return unreachable;
}

// ===================
// Aggiungi archi minimi
// ===================

function makeGraphReachable(graph, start) {
    const unreachable = findUnreachableNodes(graph, start);
    const addedEdges = [];

    for (let node of unreachable) {
        graph.addEdge(start, node);
        addedEdges.push([start, node]);
    }

    return addedEdges;
}

// ===================
// Esecuzione del programma
// ===================

const numNodes = 8; // Cambia qui per dimensioni diverse
const startNode = 0;

const graph = generateRandomGraph(numNodes);
graph.print();

const unreachable = findUnreachableNodes(graph, startNode);
console.log("\nNodi non raggiungibili da nodo", startNode, ":", unreachable);

const added = makeGraphReachable(graph, startNode);

if (added.length > 0) {
    console.log("\nArchi aggiunti per rendere il grafo raggiungibile:");
    for (let [from, to] of added) {
        console.log(`${from} -> ${to}`);
    }
} else {
    console.log("\nIl grafo è già completamente raggiungibile dal nodo", startNode);
}

console.log("\nGrafo aggiornato:");
graph.print();
