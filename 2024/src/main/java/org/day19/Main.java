package org.day19;

import java.util.Arrays;
import java.util.regex.Pattern;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day19.txt";

  private Main() {
    super();
  }

  private static String[][] parseInput(final String input) {
    final String[] parts = input.split(System.lineSeparator() + System.lineSeparator());
    return new String[][]{
        parts[0].split(", "),
        parts[1].split(System.lineSeparator())
    };
  }

  private static Pattern buildPattern(final String[] towels) {
    final StringBuilder patternBuilder = new StringBuilder();
    patternBuilder.append("(");
    for (final String towel : towels) {
      patternBuilder.append(towel);
      patternBuilder.append("|");
    }
    patternBuilder.deleteCharAt(patternBuilder.length() - 1);
    patternBuilder.append(")+");
    return Pattern.compile(patternBuilder.toString());
  }

  private static long possibleDesigns(final String[] towels, final String[] designs) {
    final Pattern pattern = buildPattern(towels);
    return Arrays.stream(designs)
        .filter(design -> pattern.matcher(design).matches())
        .count();
  }

  private static long getNumberOfDifferentWays(final String[] towels, final String design) {
    final long[] dp = new long[design.length() + 1];
    dp[design.length()] = 1L;
    for (int i = design.length() - 1; i >= 0; i--) {
      for (final String towel : towels) {
        if (design.startsWith(towel, i)) {
          dp[i] += dp[i + towel.length()];
        }
      }
    }
    return dp[0];
  }

  private static long sumNumberOfDifferentWays(final String[] towels, final String[] designs) {
    return Arrays.stream(designs)
        .reduce(
            0L,
            (acc, curr) -> acc + getNumberOfDifferentWays(towels, curr),
            Long::sum
        );
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final String[][] parsedInput = parseInput(input);
    final String[] towels = parsedInput[0];
    final String[] designs = parsedInput[1];
    final long result1 = possibleDesigns(towels, designs);
    System.out.println("Part 1: " + result1);
    final long result2 = sumNumberOfDifferentWays(towels, designs);
    System.out.println("Part 2: " + result2);
  }
}
