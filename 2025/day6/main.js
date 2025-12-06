import { input } from "./input.js";

const problems = input.split("\n").map((x, i, arr) => x.trim().split(/\s+/).map(y => i < arr.length - 1 ? parseInt(y) : y));

function solvePart1(input) {
    let ret = new Array(input[0].length).fill(0);
    for (let j = 0; j < input[0].length; j++) {
        ret[j] = input[0][j];
    }
    for (let i = 1; i < input.length - 1; i++) {
        for (let j = 0; j < input[i].length; j++) {
            if (input[input.length - 1][j] === "+") {
                ret[j] += input[i][j];
            } else {
                ret[j] *= input[i][j];
            }
        }
    }
    return ret.reduce((acc, curr) => acc + curr, 0);
}

function readNumber(input, col) {
    let ret = 0;
    for (let i = 0; i < input.length; i++) {
        if (input[i][col] >= "0" && input[i][col] <= "9") {
            ret = 10 * ret + parseInt(input[i][col]);
        } 
    }
    return ret;
}

function solvePart2(input) {
    const lines = input.split("\n");
    const problems = [];
    for (let i = 0; i < lines[lines.length - 1].length; i++) {
        if (lines[lines.length - 1][i] === "+" || lines[lines.length - 1][i] === "*") {
            problems.push(i);
        }
    }
    let ret = 0;
    for (let i = 0; i < problems.length; i++) {
        const start = problems[i];
        const end = i + 1 < problems.length ? problems[i + 1] - 1 : lines[0].length;
        let sol = readNumber(lines, start);
        for (let j = start + 1; j < end; j++) {
            if (lines[lines.length - 1][start] === "+") {
                sol += readNumber(lines, j);
            } else {
                sol *= readNumber(lines, j);
            }
        }
        ret += sol;
    }
    return ret;
}

console.log("Part 1:", solvePart1(problems));
console.log("Part 2:", solvePart2(input));