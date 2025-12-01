import { input } from "./input.js";

const rotations = input.split("\n").map(x => {
    return [x[0], parseInt(x.substring(1))];
});

const test = [];

function solvePart1(rotations) {
    let curr = 50;
    let password = 0;
    for (let i = 0; i < rotations.length; i++) {
        const [dir, amount] = rotations[i];
        if (dir === "L") {
            curr -= amount;
        } else {
            curr += amount;
        }
        if (curr > 99) {
            curr = curr % 100;
        }
        while (curr < 0) {
            curr += 100;
        }
        test.push(curr);
        if (curr === 0) {
            password++;
        }
    }
    return password;
}

function solvePart2(rotations) {
    let curr = 50;
    let password = 0;
    let j = 0;
    for (let i = 0; i < rotations.length; i++) {
        let prev = curr;
        let [dir, amount] = rotations[i];
        password += Math.floor(amount / 100);
        amount = amount % 100;
        if (dir === "L") {
            curr -= amount;
        } else {
            curr += amount;
        }
        if (curr === 0) {
            password++;
        }
        if (curr > 99) {
            password++;
            curr -= 100;
        }
        if (curr < 0) {
            if (prev !== 0) {
                password++;
            }
            curr += 100;
        }
    }
    return password;
}

console.log("Part 1:", solvePart1(rotations));
console.log("Part 2:", solvePart2(rotations));