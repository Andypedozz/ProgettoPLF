// Functions for first traversal (to get postOrder)

function DFS(vs = [], as = [], start) {
    const visited = [];
    const stack = [];

    if (vs.includes(start)) {
        dfs(start, vs, as, visited, stack);
    }

    return stack;
}

function dfs(u, vs, as, visited, stack = []) {
    visited.push(u);
    const nei = neighbors(u, as);
    nei.forEach(node => {
        if (!visited.includes(node)) {
            dfs(node, vs, as, visited, stack);
        }
    });

    if (stack) stack.push(u);
}

function neighbors(node, as = []) {
    const neighbors = [];

    as.forEach(edge => {
        if (edge[0] === node) neighbors.push(edge[1]);
    });

    return neighbors;
}

// Main function: get strongly connected components
function getSCCs(vs = [], reversedAs = [], postOrder = []) {
    const visited = [];
    const sccs = [];

    // Iteriamo i nodi in ordine inverso rispetto al postOrder
    for (let i = postOrder.length - 1; i >= 0; i--) {
        const node = postOrder[i];

        if (!visited.includes(node)) {
            const component = [];
            dfsSCC(node, vs, reversedAs, visited, component);
            sccs.push(component);
        }
    }

    return sccs;
}

// DFS speciale per costruire le componenti
function dfsSCC(u, vs, as, visited, component) {
    visited.push(u);
    component.push(u);

    const nei = neighbors(u, as);
    nei.forEach(node => {
        if (!visited.includes(node)) {
            dfsSCC(node, vs, as, visited, component);
        }
    });
}

function kosaraju(vs, as, start = vs[0]) {
    // Getting completing order of visit
    const postOrder = DFS(vs, as, start);

    // Reversing the edges
    const reversedAs = [];
    as.forEach(edge => {
        reversedAs.push([edge[1], edge[0]]);
    });

    // Getting SCCs
    const sccs = getSCCs(vs, reversedAs, postOrder);
    return sccs;
}

// ===================
// ESEMPIO DI UTILIZZO
// ===================

let graphs = [
  { vs: [1,2,3,4], as: [[1,2],[2,3],[3,4],[4,1]] },
  { vs: [1,2,3,4], as: [[1,2],[2,1],[3,4],[4,3]] },
  { vs: [1,2,3,4], as: [[1,2],[2,3],[3,4]] },
  { vs: [1,2,3,4,5], as: [[1,2],[2,3],[3,1],[3,4],[4,5],[5,3]] },
  { vs: [1,2,3,4,5,6], as: [[1,2],[2,3],[3,1],[4,5],[5,6],[6,4],[3,4]] },
  { vs: [1,2,3,4,5], as: [[1,2],[2,1],[3,3],[4,5]] },
  { vs: [1,2,3,4,5], as: [[1,2],[2,3],[3,1],[3,4],[4,5]] },
  { vs: [1,2,3,4,5,6], as: [[1,2],[2,1],[2,3],[3,4],[4,3],[4,5],[5,6],[6,5]] },
  { vs: [1,2,3,4,5,6], as: [[1,2],[2,3],[3,4],[4,5],[5,6]] },
  { vs: [1,2,3,4,5,6,7,8,9], as: [[1,2],[2,3],[3,1],[3,4],[4,5],[5,6],[6,4],[6,7],[7,8],[8,7],[8,9]] }
];

graphs.forEach(graph => {
    const { vs, as } = graph;
    console.log(kosaraju(vs, as));
});
