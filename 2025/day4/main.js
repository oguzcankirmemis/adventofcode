import { input } from "./input.js"

const map = input.split("\n").map(x => x.split(""));

function countNeighbours(input, i, j) {
    const dirs = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]];
    return dirs.filter(dir => {
        const ni = i + dir[0];
        const nj = j + dir[1];
        if (ni < 0 || ni >= input.length || nj < 0 || nj >= input[ni].length) {
            return false;
        }
        return input[ni][nj] === "@";
    }).length;
}

function solvePart1(input) {
    let ret = 0;
    for (let i = 0; i < input.length; i++) {
        for (let j = 0; j < input[i].length; j++) {
            if (input[i][j] === "@" && countNeighbours(input, i, j) < 4) {
                ret++;
            }
        }
    }
    return ret;
}

function solvePart2(input) {
    let ret = 0;
    let toRemove = [];
    do {
        ret += toRemove.length;
        toRemove.forEach(x => input[x[0]][x[1]] = ".");
        toRemove = [];
        for (let i = 0; i < input.length; i++) {
            for (let j = 0; j < input[i].length; j++) {
                if (input[i][j] === "@" && countNeighbours(input, i, j) < 4) {
                    toRemove.push([i, j]);
                }
            }
        }
    } while (toRemove.length !== 0);
    return ret;
}

console.log("Part 1:", solvePart1(map));
console.log("Part 2:", solvePart2(map));