
function kosaraju(graph) {
    const { nodes, edges} = graph;

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

const graph = {
    nodes: [0,1,2,3,4,5,6,7],
    edges: [
        [0,1],
        [1,2],
        [2,0],
        [2,3],
        [3,4],
        [4,5],
        [4,7],
        [5,6],
        [6,4],
        [6,7]
    ]
}

const sccs = kosaraju(graph);
console.log(sccs);