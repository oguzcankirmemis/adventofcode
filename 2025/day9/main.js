import { input } from "./input.js";

const coordinates = input.split("\n").map(x => x.split(",").map(y => parseInt(y)));

function solvePart1(coordinates) {
    let ret = 0;
    for (let i = 0; i < coordinates.length; i++) {
        const [x1, y1] = coordinates[i];
        for (let j = i + 1; j < coordinates.length; j++) {
            const [x2, y2] = coordinates[j];
            const dx = Math.abs(x1 - x2) + 1;
            const dy = Math.abs(y1 - y2) + 1;
            ret = Math.max(ret, dx * dy);
        }
    }
    return ret;
}

function sutherlandHodgman(polygon, rectangle) {
    let outputList = polygon;
    for (let i = 0; i < rectangle.edges.length; i++) {
        const edge = rectangle.edges[i];
        const inputList = outputList;
        outputList = [];
        for (let j = 0; j < inputList.length; j++) {
            const currentPoint = inputList[j];
            const prevPoint = j > 0 ? inputList[j - 1] : inputList[inputList.length - 1];
            if (edge.horizontal) {
                if (edge.isInside(currentPoint[1])) {
                    if (!edge.isInside(prevPoint[1]) && currentPoint[1] !== edge.y) {
                        outputList.push([currentPoint[0], edge.y]);
                    }
                    outputList.push(currentPoint);
                } else if (edge.isInside(prevPoint[1])) {
                    outputList.push([currentPoint[0], edge.y]);
                }
            } else {
                if (edge.isInside(currentPoint[0])) {
                    if (!edge.isInside(prevPoint[0]) && currentPoint[0] !== edge.x) {
                        outputList.push([edge.x, currentPoint[1]]);
                    }
                    outputList.push(currentPoint);
                } else if (edge.isInside(prevPoint[0])) {
                    outputList.push([edge.x, currentPoint[1]]);
                }
            }
        }
    }
    const rectanglePoints = [
        {
            point: [rectangle.x[0], rectangle.y[0]],
            found: false,
        }, {
            point: [rectangle.x[0], rectangle.y[1]],
            found: false,
        }, {
            point: [rectangle.x[1], rectangle.y[0]],
            found: false,
        }, {
            point: [rectangle.x[1], rectangle.y[1]],
            found: false,
        }
    ]
    for (let i = 0; i < rectanglePoints.length; i++) {
        if (rectanglePoints[i].point[0] === outputList[0][0] && rectanglePoints[i].point[1] === outputList[0][1]) {
            rectanglePoints[i].found = true;
        }
    }
    let horizontal = outputList[0][1] === outputList[1][1];
    for (let i = 1; i < outputList.length; i++) {
        const [x1, y1] = outputList[i];
        const [x2, y2] = outputList[(i + 1) % outputList.length];
        if ((horizontal && y1 !== y2) || (!horizontal && x1 !== x2)) {
            horizontal = !horizontal;
            let found = false;
            for (let j = 0; j < rectanglePoints.length; j++) {
                if (rectanglePoints[j].point[0] === x1 && rectanglePoints[j].point[1] === y1) {
                    rectanglePoints[j].found = true;
                    found = true;
                }
            }
            if (!found) {
                return false;
            }
        }
    }
    return rectanglePoints.every(r => r.found);
}

function solvePart2(coordinates) {
    const rectangles = [];
    for (let i = 0; i < coordinates.length; i++) {
        const [x1, y1] = coordinates[i];
        for (let j = i + 1; j < coordinates.length; j++) {
            const [x2, y2] = coordinates[j];
            const dx = Math.abs(x1 - x2) + 1;
            const dy = Math.abs(y1 - y2) + 1;
            const area = dx * dy;
            const edges = [
                {
                    horizontal: true,
                    x: x1 < x2 ? [x1, x2] : [x2, x1],
                    y: y1,
                    isInside: y => y1 < y2 ? y >= y1 : y <= y1,
                }, {
                    horizontal: true,
                    x: x1 < x2 ? [x1, x2] : [x2, x1],
                    y: y2,
                    isInside: y => y2 < y1 ? y >= y2 : y <= y2,
                }, {
                    horizontal: false,
                    x: x1,
                    y: y1 < y2 ? [y1, y2] : [y2, y1],
                    isInside: x => x1 < x2 ? x >= x1 : x <= x1,
                }, {
                    horizontal: false,
                    x: x2,
                    y: y1 < y2 ? [y1, y2] : [y2, y1],
                    isInside: x => x2 < x1 ? x >= x2 : x <= x2,
                }
            ];
            rectangles.push({
                x: x1 < x2 ? [x1, x2] : [x2, x1],
                y: y1 < y2 ? [y1, y2] : [y2, y1],
                area: area,
                edges: edges,
            });
        }
    }
    rectangles.sort((r1, r2) => r2.area - r1.area);
    for (let i = 0; i < rectangles.length; i++) {
        if (sutherlandHodgman(coordinates, rectangles[i])) {
            return rectangles[i].area;
        }
    }
    return -1;
}

console.log("Part 1:", solvePart1(coordinates));
console.log("Part 2:", solvePart2(coordinates));