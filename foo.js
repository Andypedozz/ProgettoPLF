// Rappresentiamo il grafo come una mappa da nodo -> lista di nodi adiacenti
// es. const graph = { A: ['B'], B: ['C'], C: ['A'], D: ['C'] }

// Funzione pura per calcolare le SCC tramite Tarjan
const tarjan = graph => {
    let index = 0;
    const stack = [];
    const indices = new Map();
    const lowlinks = new Map();
    const onStack = new Set();
    const sccs = [];

    const strongConnect = v => {
        indices.set(v, index);
        lowlinks.set(v, index);
        index++;
        stack.push(v);
        onStack.add(v);

        (graph[v] || []).forEach(w => {
            if (!indices.has(w)) {
                strongConnect(w);
                lowlinks.set(v, Math.min(lowlinks.get(v), lowlinks.get(w)));
            } else if (onStack.has(w)) {
                lowlinks.set(v, Math.min(lowlinks.get(v), indices.get(w)));
            }
        });

        if (lowlinks.get(v) === indices.get(v)) {
            let scc = [];
            let w;
            do {
                w = stack.pop();
                onStack.delete(w);
                scc.push(w);
            } while (w !== v);
            sccs.push(scc);
        }
    };

    Object.keys(graph).forEach(v => {
        if (!indices.has(v)) strongConnect(v);
    });

    return sccs;
};

// Costruisci il grafo condensato delle SCC
const buildCondensedGraph = (graph, sccs) => {
    // Mappa nodo -> indice della sua SCC
    const nodeToScc = {};
    sccs.forEach((component, idx) => {
        component.forEach(node => {
            nodeToScc[node] = idx;
        });
    });

    // Creiamo il grafo condensato, rappresentato come mappa scc -> set di scc adiacenti
    const condensed = Array(sccs.length).fill(null).map(() => new Set());

    Object.entries(graph).forEach(([u, neighbors]) => {
        neighbors.forEach(v => {
            const sccU = nodeToScc[u];
            const sccV = nodeToScc[v];
            if (sccU !== sccV) {
                condensed[sccU].add(sccV);
            }
        });
    });

    return { condensed, nodeToScc };
};

// Calcola il numero di SCC senza archi entranti raggiungibili da S
const countZeroIndegreeSCC = (condensed, nodeToScc, sccs, S) => {
    const n = sccs.length;
    const indegree = Array(n).fill(0);

    condensed.forEach((neighbors, u) => {
        neighbors.forEach(v => {
            indegree[v]++;
        });
    });

    // Individua la SCC di partenza
    const startScc = nodeToScc[S];

    // Per capire quali SCC sono raggiungibili da quella di partenza
    // facciamo DFS sul grafo condensato
    const visited = new Set();
    const dfs = u => {
        if (visited.has(u)) return;
        visited.add(u);
        condensed[u].forEach(v => dfs(v));
    };
    dfs(startScc);

    // Conta quante SCC raggiungibili hanno indegree 0
    // (escludiamo la SCC di partenza stessa che ovviamente ha indegree 0 o no)
    // In realtà, se la SCC di partenza ha indegree 0 è ok,
    // ma vogliamo aggiungere archi per collegare tutte le altre SCC raggiungibili ma con indegree 0
    // E le SCC non raggiungibili da S non sono considerate, perché non servono per la raggiungibilità da S.

    let count = 0;
    for (let i = 0; i < n; i++) {
        if (visited.has(i) && indegree[i] === 0 && i !== startScc) {
            count++;
        }
    }

    return count;
};

// Funzione principale: input grafo e S, output numero minimo di archi da aggiungere
const minEdgesToAddForReachability = (graph, S) => {
    const sccs = tarjan(graph);
    const { condensed, nodeToScc } = buildCondensedGraph(graph, sccs);
    return countZeroIndegreeSCC(condensed, nodeToScc, sccs, S);
};

// Esempio
const graph = {
    A: ['B'],
    B: ['C'],
    C: ['A', 'D'],
    D: ['E'],
    E: []
};

const S = 'A';

console.log(minEdgesToAddForReachability(graph, S)); // Output: 0 (tutti raggiungibili da A)

// Se vuoi provare con un grafo disconnesso:
const graph2 = {
    A: ['B'],
    B: [],
    C: ['D'],
    D: []
};

console.log(minEdgesToAddForReachability(graph2, 'A')); // Output: 1 (serve un arco per raggiungere C e D)
