import { input } from "./input.js";

const inputParts = input.split("\n\n");

const ranges = inputParts[0].split("\n").map(x => x.split("-").map(y => parseInt(y)));
const ingredients = inputParts[1].split("\n").map(x => parseInt(x));

function solvePart1(ingredients, ranges) {
    let ret = 0;
    for (let i = 0; i < ingredients.length; i++) {
        for (let j = 0; j < ranges.length; j++) {
            if (ranges[j][0] <= ingredients[i] && ingredients[i] <= ranges[j][1]) {
                ret++;
                break;
            }
        }
    }
    return ret;
}

function solvePart2(ranges) {
    const sorted = ranges.sort((r1, r2) => {
        if (r1[0] < r2[0]) {
            return -1;
        }
        if (r2[0] < r1[0]) {
            return 1;
        }
        if (r1[1] < r2[0]) {
            return -1;
        }
        if (r2[1] < r1[0]) {
            return -1;
        }
        return 0;
    });
    const stack = [sorted[0]];
    for (let i = 1; i < sorted.length; i++) {
        let toInsert = sorted[i];
        while (stack.length > 0 && stack[stack.length - 1][1] >= toInsert[0]) {
            const last = stack.pop();
            toInsert[0] = Math.min(toInsert[0], last[0]);
            toInsert[1] = Math.max(toInsert[1], last[1]);
        }
        stack.push(toInsert);
    }
    return stack.reduce((acc, curr) => acc + curr[1] - curr[0] + 1, 0);
}

console.log("Part 1:", solvePart1(ingredients, ranges));
console.log("Part 2:", solvePart2(ranges));