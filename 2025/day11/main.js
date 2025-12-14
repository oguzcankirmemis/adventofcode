import { input } from "./input.js";

function parse(input) {
    const ret = new Map();
    const edges = input.split("\n");
    for (let i = 0; i < edges.length; i++) {
        const parts = edges[i].split(": ");
        ret.set(parts[0], parts[1].split(" "));
    }
    return ret;
}

const edges = parse(input);

function solvePart1(input) {
    let ret = 0;
    const stack = ["you"];
    while (stack.length > 0) {
        const node = stack.pop();
        if (node === "out") {
            ret++;
            continue;
        }
        const neighbours = input.get(node);
        if (!neighbours) {
            continue;
        }
        for (let i = 0; i < neighbours.length; i++) {
            stack.push(neighbours[i]);
        }
    }
    return ret;
}

function bfs(input, nodeSet, nodeIdMap, reverseInput, start, end) {
    let dp = new Array(nodeSet.size).fill(null).map(_ => new Map());
    for (const [node1, neighbours] of input) {
        for (const node2 of neighbours) {
            const idNode2 = nodeIdMap.get(node2);
            dp[idNode2].set(node1, 1);
        }
    }
    const idEnd = nodeIdMap.get(end);
    while (!dp[idEnd].has(start) && dp[idEnd].size > 0) {
        const nextDp = new Array(nodeSet.size).fill(null).map(_ => new Map());
        for (let id1 = 0; id1 < dp.length; id1++) {
            for (const [node2, numOfPaths] of dp[id1]) {
                if (!reverseInput.has(node2)) {
                    continue;
                }
                for (const node3 of reverseInput.get(node2)) {
                    if (!nextDp[id1].has(node3)) {
                        nextDp[id1].set(node3, 0);
                    }
                    const previousNumOfPaths = nextDp[id1].get(node3);
                    nextDp[id1].set(node3, previousNumOfPaths + numOfPaths);
                }
            }
        }
        dp = nextDp;
    }
    if (dp[idEnd].size === 0) {
        return 0;
    }
    let ret = 0;
    while (dp[idEnd].has(start)) {
        ret += dp[idEnd].get(start);
        const nextDp = new Array(nodeSet.size).fill(null).map(_ => new Map());
        for (let id1 = 0; id1 < dp.length; id1++) {
            for (const [node2, numOfPaths] of dp[id1]) {
                if (!reverseInput.has(node2)) {
                    continue;
                }
                for (const node3 of reverseInput.get(node2)) {
                    if (!nextDp[id1].has(node3)) {
                        nextDp[id1].set(node3, 0);
                    }
                    const previousNumOfPaths = nextDp[id1].get(node3);
                    nextDp[id1].set(node3, previousNumOfPaths + numOfPaths);
                }
            }
        }
        dp = nextDp;
    }
    return ret;
}

function solvePart2(input, start, checkpoint1, checkpoint2, end) {
    let id = 0;
    const nodeSet = new Set();
    const nodeIdMap = new Map();
    const reverseInput = new Map();
    for (const [node1, neighbours] of input) {
        for (const node2 of neighbours) {
            if (!nodeSet.has(node2)) {
                nodeSet.add(node2);
                nodeIdMap.set(node2, id);
                id++;
            }
            if (!reverseInput.has(node2)) {
                reverseInput.set(node2, new Set());
            }
            reverseInput.get(node2).add(node1);
        }
        if (!nodeSet.has(node1)) {
            nodeSet.add(node1);
            nodeIdMap.set(node1, id);
            id++;
        }
    }
    let betweenCheckpoints = bfs(input, nodeSet, nodeIdMap, reverseInput, checkpoint1, checkpoint2);
    if (betweenCheckpoints === 0) {
        [checkpoint1, checkpoint2] = [checkpoint2, checkpoint1];
        betweenCheckpoints = bfs(input, nodeSet, nodeIdMap, reverseInput, checkpoint1, checkpoint2);
    }
    const betweenStartAndCheckpoint = bfs(input, nodeSet, nodeIdMap, reverseInput, start, checkpoint1);
    const betweenCheckpointAndEnd = bfs(input, nodeSet, nodeIdMap, reverseInput, checkpoint2, end);
    return betweenStartAndCheckpoint * betweenCheckpoints * betweenCheckpointAndEnd;
}


console.log("Part 1:", solvePart1(edges));
console.log("Part 2:", solvePart2(edges, "svr", "fft", "dac", "out"));