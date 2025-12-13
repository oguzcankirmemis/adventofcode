import { input } from "./input.js";

function parseMachine(input) {
    const parts = input.split(" ");
    const config = parts[0].substring(1, parts[0].length - 1).split("").reduce((acc, curr, i) => curr === "#" ? acc + Math.pow(2, i) : acc, 0);
    const allOne = Math.pow(2, parts[0].length - 2) - 1;
    const buttons = parts.slice(1, parts.length - 1)
        .map(x => x.substring(1, x.length - 1)
            .split(",")
            .map(y => parseInt(y))
            .map(y => Math.pow(2, y))
            .reduce((a, c) => a + c))
        .map(x => {
            return {
                mask: x,
                reverse: allOne - x,
            };
        });
    const increments = parts.slice(1, parts.length - 1)
        .map(x => x.substring(1, x.length - 1)
            .split(",")
            .map(y => parseInt(y)));
    const joltages = parts[parts.length - 1].substring(1, parts[parts.length - 1].length - 1).split(",").map(x => parseInt(x));
    return {
        config: config,
        buttons: buttons,
        increments: increments,
        joltages: joltages,
    };
}

function dfs(machine) {
    let ret = Number.MAX_SAFE_INTEGER;
    const stack = [[0, 0]];
    const explored = new Map();
    explored.set(0, 0);
    while (stack.length > 0) {
        const [setting, presses] = stack.pop();
        if (presses >= ret) {
            continue;
        }
        if (setting === machine.config) {
            ret = presses;
            continue;
        }
        for (let i = 0; i < machine.buttons.length; i++) {
            const button = machine.buttons[i];
            const newSetting = (setting & button.reverse) | ((setting ^ button.mask) & button.mask);
            const newPresses = presses + 1;
            if (!explored.has(newSetting) || explored.get(newSetting) > newPresses) {
                stack.push([newSetting, newPresses]);
                explored.set(newSetting, newPresses);
            }
        }
    }
    return ret;
}

function createMatrix(machine) {
    const matrix = new Array(machine.joltages.length).fill(null).map(_ => new Array(machine.increments.length).fill(0));
    for (let i = 0; i < machine.increments.length; i++) {
        const button = machine.increments[i];
        for (let j = 0; j < button.length; j++) {
            matrix[button[j]][i] = 1;
        }
    }
    return matrix;
}

function gcd(a, b) {
    if (b === 0) {
        return a;
    }
    return gcd(b, a % b);
}

function gaussianElimination(matrix, machine) {
    const resultVector = machine.joltages.map(j => j);
    let pivotRow = 0;
    let pivotCol = 0;
    while (pivotRow < matrix.length && pivotCol < matrix[0].length) {
        let iMax = pivotRow;
        for (let i = pivotRow; i < matrix.length; i++) {
            if (Math.abs(matrix[i][pivotCol]) > Math.abs(matrix[iMax][pivotCol])) {
                iMax = i;
            }
        }
        if (matrix[iMax][pivotCol] == 0) {
            pivotCol++;
            continue;
        }
        let tmp = matrix[pivotRow];
        matrix[pivotRow] = matrix[iMax];
        matrix[iMax] = tmp;
        tmp = resultVector[pivotRow];
        resultVector[pivotRow] = resultVector[iMax];
        resultVector[iMax] = tmp;
        for (let i = pivotRow + 1; i < matrix.length; i++) {
            if (matrix[i][pivotCol] === 0) {
                continue;
            }
            const greatestCommonDivisor = gcd(Math.abs(matrix[i][pivotCol]), Math.abs(matrix[pivotRow][pivotCol]));
            let pivotRowScalar = matrix[i][pivotCol] / greatestCommonDivisor;
            const targetRowScalar = matrix[pivotRow][pivotCol] / greatestCommonDivisor;
            for (let j = 0; j < matrix[i].length; j++) {
                matrix[pivotRow][j] *= pivotRowScalar; 
                matrix[i][j] *= targetRowScalar;
            }
            resultVector[pivotRow] *= pivotRowScalar;
            resultVector[i] *= targetRowScalar;
            matrix[i][pivotCol] = 0;
            for (let j = pivotCol + 1; j < matrix[i].length; j++) {
                matrix[i][j] -= matrix[pivotRow][j];
            }
            resultVector[i] -= resultVector[pivotRow];
        }
        pivotRow++;
        pivotCol++;
    }
    return [matrix, resultVector];
}

function backtrack(matrix, resultVector, currentSolution, row, machine) {
    if (row < 0) {
        let ret = 0;
        for (let i = 0; i < currentSolution.length; i++) {
            if (currentSolution[i] != -1) {
                ret += currentSolution[i];
            }
        }
        return ret;
    }
    let i = 0;
    while (i < matrix[row].length && matrix[row][i] === 0) {
        i++;
    }
    if (i >= matrix[row].length) {
        return backtrack(matrix, resultVector, currentSolution, row - 1, machine);
    }
    let result = resultVector[row];
    const toBruteForce = [];
    for (let j = i + 1; j < matrix[row].length; j++) {
        if (matrix[row][j] !== 0 && currentSolution[j] === -1) {
            toBruteForce.push(j);
        } else {
            result -= matrix[row][j] * currentSolution[j];
        }
    }
    let solution = Number.MAX_SAFE_INTEGER;
    const stack = [new Array(toBruteForce.length).fill(0)];
    const explored = new Set([stack[stack.length - 1].join("#")]);
    while (stack.length > 0) {
        const newCurrentSolution = currentSolution.map(c => c);
        const el = stack.pop();
        let sum = 0;
        for (let j = 0; j < el.length; j++) {
            sum += el[j] * matrix[row][toBruteForce[j]]
            newCurrentSolution[toBruteForce[j]] = el[j];
        }
        const diff = result - sum;
        if (diff % matrix[row][i] === 0 && diff / matrix[row][i] >= 0) {
            newCurrentSolution[i] = diff / matrix[row][i];
            solution = Math.min(solution, backtrack(matrix, resultVector, newCurrentSolution, row - 1, machine));
        }
        const joltages = machine.joltages.map(j => j);
        for (let j = 0; j < toBruteForce.length; j++) {
            const button = machine.increments[toBruteForce[j]];
            for (let k = 0; k < button.length; k++) {
                joltages[button[k]] -= el[j];
            }
        }
        for (let j = 0; j < el.length; j++) {
            const button = machine.increments[toBruteForce[j]];
            if (button.every(b => joltages[b] > 0)) {
                const newJoltages = joltages.map(j => j);
                const elCopy = el.map(j => j);
                elCopy[j]++;
                const hash = elCopy.join("#");
                if (explored.has(hash)) {
                    continue;
                }
                explored.add(hash);
                for (let k = 0; k < button.length; k++) {
                    newJoltages[button[k]]--;
                }
                stack.push(elCopy);
            }
        }
    }
    return solution;
}

function solvePart1(machines) {
    let ret = 0;
    for (let i = 0; i < machines.length; i++) {
        ret += dfs(machines[i]);
    }
    return ret;
}

function solvePart2(machines) {
    let ret = 0;
    for (let i = 0; i < machines.length; i++) {
        const inputMatrix = createMatrix(machines[i]);
        const [matrix, resultVector] = gaussianElimination(inputMatrix, machines[i]);
        ret += backtrack(matrix, resultVector, new Array(matrix[0].length).fill(-1), matrix.length - 1, machines[i]);
    }
    return ret;
}

const machines = input.split("\n").map(parseMachine);

console.log("Part 1:", solvePart1(machines));
console.log("Part 2:", solvePart2(machines));