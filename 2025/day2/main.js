import { input } from "./input.js"

const idRanges = input.split(",").map(x => x.split("-").map(y => parseInt(y)));

function solveRangePart1(from, to) {
    let ret = 0;
    for (let i = from; i <= to; i++) {
        let numStr = "" + i;
        if (numStr.length % 2 === 0 && numStr.substring(0, numStr.length / 2) === numStr.substring(numStr.length / 2)) {
            ret += i;
        }
    }
    return ret;
}

function solveRangePart2(from, to) {
    let ret = 0;
    for (let i = from; i <= to; i++) {
        let numStr = "" + i;
        for (let j = 1; j <= numStr.length / 2; j++) {
            if (numStr.length % j === 0) {
                let part = numStr.substring(0, j);
                let repeating = true;
                for (let k = j; k < numStr.length; k += j) {
                    if (numStr.substring(k, k + j) !== part) {
                        repeating = false;
                        break;
                    }
                }
                if (repeating) {
                    ret += i;
                    break;
                }
            }
        }
    }
    return ret;
}

function solvePart1(input) {
    let ret = 0;
    for (let i = 0; i < input.length; i++) {
        ret += solveRangePart1(input[i][0], input[i][1]);
    }
    return ret;
}

function solvePart2(input) {
    let ret = 0;
    for (let i = 0; i < input.length; i++) {
        ret += solveRangePart2(input[i][0], input[i][1]);
    }
    return ret;
}

console.log("Part 1:", solvePart1(idRanges));
console.log("Part 2:", solvePart2(idRanges));