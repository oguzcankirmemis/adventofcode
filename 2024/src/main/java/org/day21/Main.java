package org.day21;

import static org.day21.DP.dp;

import java.util.Arrays;
import java.util.List;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day21.txt";

  private Main() {
    super();
  }

  private static String[] parseInput(final String input) {
    return input.split(System.lineSeparator());
  }

  private static long getNumericValue(final String code) {
    return Long.parseLong(
        code.chars()
            .filter(Character::isDigit)
            .mapToObj(c -> (char) c)
            .reduce("", (String acc, Character curr) -> acc + curr, String::concat)
    );
  }

  private static long lengthShortestSequence(final String code, final int numOfIterations) {
    final NumericKeypad numKeypad = new NumericKeypad();
    final DirectionalKeypad dirKeypad = new DirectionalKeypad();
    final List<List<Direction>> result = numKeypad.pressCode(code);
    final long[][][] dpTable = dp(dirKeypad, numOfIterations);
    long shortest = Long.MAX_VALUE;
    for (final List<Direction> instructions : result) {
      long subResult = 0L;
      Direction curr = Direction.ACTIVATE;
      for (final Direction dir : instructions) {
        final int currKey = curr.code % Direction.NUM_DIRECTIONS;
        final int dirKey = dir.code % Direction.NUM_DIRECTIONS;
        subResult += dpTable[numOfIterations][currKey][dirKey];
        curr = dir;
      }
      shortest = Math.min(shortest, subResult);
    }
    return shortest;
  }

  private static long lengthShortestSequenceBruteforce(final String code) {
    final NumericKeypad keypad1 = new NumericKeypad();
    final DirectionalKeypad keypad2 = new DirectionalKeypad();
    final DirectionalKeypad keypad3 = new DirectionalKeypad();
    final List<List<Direction>> result1 = keypad1.pressCode(code);
    final List<List<Direction>> result2 = keypad2.pressCodeShortest(result1);
    final List<List<Direction>> result3 = keypad3.pressCodeShortest(result2);
    return result3.getFirst().size();
  }

  private static long computeComplexity(final String code) {
    return getNumericValue(code) * lengthShortestSequenceBruteforce(code);
  }

  private static long sumComplexities(final String[] codes) {
    return Arrays.stream(codes)
        .map(Main::computeComplexity)
        .reduce(0L, Long::sum);
  }

  private static long computeEnhancedComplexity(final String code, final int numOfIterations) {
    return getNumericValue(code) * lengthShortestSequence(code, numOfIterations);
  }

  private static long sumEnhancedComplexities(final String[] codes, final int numOfIterations) {
    return Arrays.stream(codes)
        .map(c -> computeEnhancedComplexity(c, numOfIterations))
        .reduce(0L, Long::sum);
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final String[] codes = parseInput(input);
    final long result1 = sumComplexities(codes);
    System.out.println("Part 1: " + result1);
    final long result2 = sumEnhancedComplexities(codes, 25);
    System.out.println("Part 2: " + result2);
  }
}
