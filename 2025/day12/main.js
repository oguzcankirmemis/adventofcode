import { input } from "./input.js";

function equalShapes(shape1, shape2) {
    for (let i = 0; i < shape1.length; i++) {
        for (let j = 0; j < shape1[i].length; j++) {
            if (shape1[i][j] !== shape2[i][j]) {
                return false;
            }
        }
    }
    return true;
}

function mirror(input) {
    const shape = new Array(input.length).fill(null).map(_ => new Array(input[0].length).fill(false));
    for (let i = 0; i < input.length; i++) {
        for (let j = 0; j < input[i].length; j++) {
            shape[input.length - 1 - i][j] = input[i][j]; 
        }
    }
    return shape;
}

function rotateRight(input) {
    const shape = new Array(input.length).fill(null).map(_ => new Array(input[0].length).fill(false));
    for (let i = 0; i < input.length; i++) {
        for (let j = 0; j < input[i].length; j++) {
            shape[j][input.length - 1 - i] = input[i][j];
        }
    }
    return shape;
}

function generateCombinations(input) {
    const possibilities = [input];
    const mirrorShape = mirror(input);
    let current = input;
    for (let i = 0; i < 3; i++) {
        current = rotateRight(current);
        if (possibilities.every(shape => !equalShapes(shape, current))) {
            possibilities.push(current);
        }
    }
    current = mirrorShape;
    for (let i = 0; i < 4; i++) {
        if (possibilities.every(shape => !equalShapes(shape, current))) {
            possibilities.push(current);
        }
        current = rotateRight(current);
    }
    return possibilities;
}

function parseShape(input) {
    const shape = input.split("\n").slice(1).map(l => l.split("").map(c => c === "#"));
    return generateCombinations(shape);
}

function parseProblem(problem) {
    const parts = problem.split(": ");
    const dimensions = parts[0].split("x").map(d => parseInt(d));
    const packages = parts[1].split(" ").map(p => parseInt(p));
    return {
        rows: dimensions[0],
        cols: dimensions[1],
        packages: packages,
    };
}

function parse(input) {
    const parts = input.split("\n\n");
    const shapes = parts.slice(0, parts.length - 1).map(p => parseShape(p));
    const problems = parts[parts.length - 1].split("\n").map(p => parseProblem(p));
    return {
        shapes: shapes,
        problems: problems,
    };
}

function place(hash, shape, row, col) {
    const newHash = hash.map(x => x);
    const baseMask = Math.pow(2, col);
    for (let i = 0; i < shape.length; i++) {
        let mask = baseMask;
        for (let j = 0; j < shape[i].length; j++) {
            if (!shape[i][j]) {
                mask = 2 * mask;
                continue;
            }
            if (Math.floor(hash[row + i] / mask) % 2 === 1) {
                return null;
            }
            newHash[row + i] += mask;
            mask = 2 * mask;
        }
    }
    return newHash;
}

function dfs(problem, shapes) {
    const startBoardHash = new Array(problem.rows).fill(0);
    const stack = [[startBoardHash, problem.packages]];
    while (stack.length > 0) {
        const [boardHash, packages] = stack.pop();
        if (packages.every(p => p === 0)) {
            return true;
        }
        for (let i = 0; i < problem.rows; i++) {
            if (i > 0 && boardHash[i - 1] === 0) {
                break;
            }
            for (let j = 0; j < problem.cols; j++) {
                if (i + shapes[0][0].length - 1 >= problem.rows || j + shapes[0][0][0].length - 1 >= problem.cols) {
                    continue;
                }
                for (let k = 0; k < packages.length; k++) {
                    if (packages[k] === 0) {
                        continue;
                    }
                    for (let l = 0; l < shapes[k].length; l++) {
                        const newBoardHash = place(boardHash, shapes[k][l], i, j);
                        if (!newBoardHash) {
                            continue;
                        }
                        const newPackages = packages.map(x => x);
                        newPackages[k]--;
                        stack.push([newBoardHash, newPackages]);
                    }
                }
            }
        }
    }
    return false;
}

function solvePart1(input) {
    let ret = 0;
    for (let i = 0; i < input.problems.length; i++) {
        let totalRequiredArea = 0;
        for (let j = 0; j < input.problems[i].packages.length; j++) {
            let count = 0;
            for (let k = 0; k < input.shapes[j][0].length; k++) {
                for (let l = 0; l < input.shapes[j][0][k].length; l++) {
                    count += input.shapes[j][0][k][l] ? 1 : 0;
                }
            }
            totalRequiredArea += count * input.problems[i].packages[j];
        }
        if (totalRequiredArea <= input.problems[i].rows * input.problems[i].cols) {
            // the check with dfs can be actually skipped because the pruning is enough due to the input
            if (dfs(input.problems[i], input.shapes)) {
                ret++;
            }
        }
    }
    return ret;
}

const configuration = parse(input);

console.log("Part 1:", solvePart1(configuration));