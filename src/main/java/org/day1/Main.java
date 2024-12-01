package org.day1;

import org.common.Toolbox;

import java.util.Arrays;
import java.util.HashMap;
import java.util.PriorityQueue;

public class Main {
    private static final String inputFile = "input_day1.txt";

    private static Long[][] parseInput(String input) {
        return Arrays.stream(
                input.split("\n")).map(line ->
                        Arrays.stream(
                                line.split("\\s+")
                        ).map(Long::valueOf).toArray(Long[]::new)
                ).toArray(Long[][]::new);
    }

    private static long solve1(Long[][] input) {
        var q1 = new PriorityQueue<Long>();
        var q2 = new PriorityQueue<Long>();
        for (Long[] element : input) {
            q1.add(element[0]);
            q2.add(element[1]);
        }
        long difference = 0L;
        while (!q1.isEmpty() && !q2.isEmpty()) {
            long num1 = q1.poll();
            long num2 = q2.poll();
            difference += Math.abs(num1 - num2);
        }
        return difference;
    }

    private static long solve2(Long[][] input) {
        var map = new HashMap<Long, Long>();
        for (Long[] element : input) {
            map.put(element[1], map.getOrDefault(element[1], 0L) + 1);
        }
        long similarity = 0L;
        for (Long[] element: input) {
            similarity += element[0] * map.getOrDefault(element[0], 0L);
        }
        return similarity;
    }

    public static void main(String[] args) {
        String input = Toolbox.getInput(inputFile);
        Long[][] parsedInput = parseInput(input);
        long result1 = solve1(parsedInput);
        System.out.println("Part 1: " + result1);
        long result2 = solve2(parsedInput);
        System.out.println("Part 2: " + result2);
    }
}