import { input } from "./input.js";

const junctions = input.split("\n").map(x => x.split(",").map(y => parseInt(y)));

// too lazy for UnionFind
function solve(junctions) {
    const edges = [];
    for (let i = 0; i < junctions.length - 1; i++) {
        for (let j = i + 1; j < junctions.length; j++) {
            const dx = (junctions[i][0] - junctions[j][0]) * (junctions[i][0] - junctions[j][0]);
            const dy = (junctions[i][1] - junctions[j][1]) * (junctions[i][1] - junctions[j][1]);
            const dz = (junctions[i][2] - junctions[j][2]) * (junctions[i][2] - junctions[j][2]);
            const distance = dx + dy + dz;
            edges.push([i, j, distance]);
        }
    }
    edges.sort((e1, e2) => e2[2] - e1[2]);
    const sets = new Array(junctions.length).fill(null).map((_, i) => new Set([i]));
    for (let l = 0; l < 1000; l++) {
        const [i, j, _] = edges.pop();
        if (!sets[i].has(j)) {
            sets[i] = sets[i].union(sets[j]);
            sets[i].forEach(k => sets[k] = sets[i]);
        }
    }
    const uniqueSets = new Set(sets);
    const sortedSets = [];
    uniqueSets.forEach(set => sortedSets.push(set));
    sortedSets.sort((s1, s2) => s2.size - s1.size);
    const part1 = sortedSets[0].size * sortedSets[1].size * sortedSets[2].size;
    while (true) {
        const [i, j, _] = edges.pop();
        if (!sets[i].has(j)) {
            sets[i] = sets[i].union(sets[j]);
            sets[i].forEach(k => sets[k] = sets[i]);
            if (sets[i].size == junctions.length) {
                return [part1, junctions[i][0] * junctions[j][0]];
            }
        }
    }
}

const [part1, part2] = solve(junctions);
console.log("Part 1:", part1);
console.log("Part 2:", part2);