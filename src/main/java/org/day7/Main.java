package org.day7;

import java.util.Arrays;
import java.util.regex.Pattern;
import org.common.Toolbox;

public class Main {

  private static final Pattern EQUATION_PATTERN = Pattern.compile(": ");
  private static final String inputFile = "input_day7.txt";

  private Main() {
    super();
  }

  private static Equation parseEquation(final String equation) {
    final String[] parts = Main.EQUATION_PATTERN.split(equation);
    final long target = Long.parseLong(parts[0]);
    final long[] operands = Arrays.stream(parts[1].split(" "))
        .mapToLong(Long::parseLong)
        .toArray();
    return new Equation(target, operands);
  }

  private static Equation[] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(Main::parseEquation)
        .toArray(Equation[]::new);
  }

  private static boolean isPossible1(final Equation equation, final long accumulated,
      final int current) {
    final long[] operands = equation.operands();
    if (current >= operands.length) {
      return equation.target() == accumulated;
    }
    if (accumulated > equation.target()) {
      return false;
    }
    return Main.isPossible1(equation, accumulated + operands[current], current + 1) ||
        Main.isPossible1(equation, accumulated * operands[current], current + 1);
  }

  private static boolean isPossible2(final Equation equation, final long accumulated,
      final int current) {
    final long[] operands = equation.operands();
    if (current >= operands.length) {
      return equation.target() == accumulated;
    }
    if (accumulated > equation.target()) {
      return false;
    }
    return Main.isPossible2(equation, accumulated + operands[current], current + 1) ||
        Main.isPossible2(equation, accumulated * operands[current], current + 1) ||
        Main.isPossible2(equation,
            Long.parseLong(Long.toString(accumulated) + Long.toString(operands[current])),
            current + 1);
  }

  private static boolean isPossible1(final Equation equation) {
    return Main.isPossible1(equation, equation.operands()[0], 1);
  }

  private static boolean isPossible2(final Equation equation) {
    return Main.isPossible2(equation, equation.operands()[0], 1);
  }

  private static long sumPossibles1(final Equation[] equations) {
    return Arrays.stream(equations)
        .filter(Main::isPossible1)
        .reduce(0L, (acc, curr) -> acc + curr.target(), Long::sum);
  }

  private static long sumPossibles2(final Equation[] equations) {
    return Arrays.stream(equations)
        .filter(Main::isPossible2)
        .reduce(0L, (acc, curr) -> acc + curr.target(), Long::sum);
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(Main.inputFile);
    final Equation[] equations = Main.parseInput(input);
    final long result1 = Main.sumPossibles1(equations);
    System.out.println("Part 1: " + result1);
    final long result2 = Main.sumPossibles2(equations);
    System.out.println("Part 2: " + result2);
  }

  private record Equation(long target, long[] operands) {

  }
}
