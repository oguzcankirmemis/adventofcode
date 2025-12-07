import { input } from "./input.js";

const map = input.split("\n");

function findStart(map) {
    for (let i = 0; i < map.length; i++) {
        for (let j = 0; j < map[i].length; j++) {
            if (map[i][j] === "S") {
                return [i, j];
            }
        }
    }
    return null;
}

function dfs1(map, i, j, memo) {
    const key = i + "#" + j;
    memo.add(key);
    let ret = 0;
    while (i < map.length && map[i][j] !== "^") {
        i++;
    }
    if (i < map.length && map[i][j] === "^") {
        const key1 = i + "#" + (j - 1);
        const key2 = i + "#" + (j + 1);
        if ((j > 0 && !memo.has(key1)) || (j + 1 < map[i].length && !memo.has(key2))) {
            ret++;
        }
        if (j > 0 && !memo.has(key1)) {
            ret += dfs1(map, i, j - 1, memo);
        }
        if (j + 1 < map[i].length && !memo.has(key2)) {
            ret += dfs1(map, i, j + 1, memo);
        }
    }
    return ret;
}

function dfs2(map, i, j, memo) {
    const key = i + "#" + j;
    if (memo.has(key)) {
        return memo.get(key);
    }
    let ret = 1;
    while (i < map.length && map[i][j] !== "^") {
        i++;
    }
    if (i < map.length && map[i][j] === "^") {
        if (j > 0 && j + 1 < map[i].length) {
            ret = dfs2(map, i, j - 1, memo) + dfs2(map, i, j + 1, memo);
        } else if (j > 0) {
            ret = dfs2(map, i, j - 1, memo);
        } else {
            ret = dfs2(map, i, j + 1, memo);
        }
    }
    memo.set(key, ret);
    return ret;
}

function solvePart1(map) {
    const [startRow, startCol] = findStart(map);
    return dfs1(map, startRow, startCol, new Set());
}

function solvePart2(map) {
    const [startRow, startCol] = findStart(map);
    return dfs2(map, startRow, startCol, new Map());
}

console.log("Part 1:", solvePart1(map));
console.log("Part 2:", solvePart2(map));