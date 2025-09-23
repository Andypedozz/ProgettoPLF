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

// ===================
// ESEMPIO DI UTILIZZO
// ===================

const vs = [0,1,2,3,4,5,6,7];
const as = [
    [0,1],
    [1,2],
    [2,0],
    [2,3],
    [3,4],
    [4,5],
    [6,4],
    [4,7],
    [5,6],
    [6,7]
];

const start = 0;

// Getting completing order of visit
const postOrder = DFS(vs, as, start);

// Reversing the edges
const reversedAs = [];
as.forEach(edge => {
    reversedAs.push([edge[1], edge[0]]);
});

// Getting SCCs
const sccs = getSCCs(vs, reversedAs, postOrder);
console.log(sccs);
