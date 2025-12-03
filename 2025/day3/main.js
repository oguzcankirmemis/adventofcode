import { input } from "./input.js"

const joltages = input.split("\n").map(x => x.split("").map(y => parseInt(y)));

function solveOne(input, digits) {
    const dp = new Array(input.length).fill(0).map(_ => new Array(digits).fill(0));
    dp[0][0] = input[0];
    for (let i = 1; i < input.length; i++) {
        dp[i][0] = Math.max(dp[i - 1][0], input[i]);
        for (let j = 1; j < Math.min(i + 1, digits); j++) {
            dp[i][j] = Math.max(dp[i - 1][j], dp[i - 1][j - 1] * 10 + input[i]);
        }
    }
    return dp[input.length - 1][digits - 1];
}

function solvePart1(input) {
    let ret = 0;
    for (let i = 0; i < input.length; i++) {
        ret += solveOne(input[i], 2);
    }
    return ret;
}

function solvePart2(input) {
    let ret = 0;
    for (let i = 0; i < input.length; i++) {
        ret += solveOne(input[i], 12);
    }
    return ret;
}

console.log("Part 1:", solvePart1(joltages));
console.log("Part 2:", solvePart2(joltages));