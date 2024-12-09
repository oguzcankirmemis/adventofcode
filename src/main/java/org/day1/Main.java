package org.day1;

import java.util.Arrays;
import java.util.HashMap;
import java.util.PriorityQueue;
import java.util.regex.Pattern;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day1.txt";
  private static final Pattern inputPattern = Pattern.compile("\\s+");

  private Main() {
    super();
  }

  private static Long[][] parseInput(final String input) {
    return Arrays.stream(
            input.split(System.lineSeparator()))
        .map(line ->
            Arrays.stream(
                    Main.inputPattern.split(line)
                )
                .map(Long::valueOf)
                .toArray(Long[]::new)
        )
        .toArray(Long[][]::new);
  }

  private static long solve1(final Long[][] input) {
    final var q1 = new PriorityQueue<Long>(input.length);
    final var q2 = new PriorityQueue<Long>(input.length);
    for (final Long[] element : input) {
      q1.add(element[0]);
      q2.add(element[1]);
    }
    long difference = 0L;
    while (!q1.isEmpty() && !q2.isEmpty()) {
      final Long num1 = q1.poll();
      final Long num2 = q2.poll();
      difference += Math.abs(num1 - num2);
    }
    return difference;
  }

  private static long solve2(final Long[][] input) {
    final var map = new HashMap<Long, Long>(input.length);
    for (final Long[] element : input) {
      map.put(element[1], map.getOrDefault(element[1], 0L) + 1);
    }
    long similarity = 0L;
    for (final Long[] element : input) {
      similarity += element[0] * map.getOrDefault(element[0], 0L);
    }
    return similarity;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(Main.inputFile);
    final Long[][] parsedInput = Main.parseInput(input);
    final long result1 = Main.solve1(parsedInput);
    System.out.println("Part 1: " + result1);
    final long result2 = Main.solve2(parsedInput);
    System.out.println("Part 2: " + result2);
  }
}