package org.day13;

import java.util.Arrays;
import java.util.Optional;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day13.txt";

  private static final String LABEL_BUTTON_A = "A";
  private static final String LABEL_BUTTON_B = "B";
  private static final long COST_BUTTON_A = 3;
  private static final long COST_BUTTON_B = 1;
  private static final long OFFSET_PART2 = 10000000000000L;

  private Main() {
    super();
  }

  private static Button parseButton(final String button) {
    final String buttonLabel = button.split(": ")[0].split(" ")[1];
    final String increments = button.split(": ")[1];
    final String[] parts = increments.split(", ");
    final long xIncrement = Long.parseLong(parts[0].split("X\\+")[1]);
    final long yIncrement = Long.parseLong(parts[1].split("Y\\+")[1]);
    if (buttonLabel.equals(LABEL_BUTTON_A)) {
      return new Button(xIncrement, yIncrement, COST_BUTTON_A);
    }
    return new Button(xIncrement, yIncrement, COST_BUTTON_B);
  }

  private static Prize parsePrize(final String prize) {
    final String coordinates = prize.split(": ")[1];
    final String[] parts = coordinates.split(", ");
    final long xPosition = Long.parseLong(parts[0].split("X=")[1]);
    final long yPosition = Long.parseLong(parts[1].split("Y=")[1]);
    return new Prize(xPosition, yPosition);
  }

  private static ClawMachine[] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator() + System.lineSeparator()))
        .map(machine -> {
          final String[] parts = machine.split(System.lineSeparator());
          return new ClawMachine(parseButton(parts[0]), parseButton(parts[1]), parsePrize(parts[2]));
        })
        .toArray(ClawMachine[]::new);
  }

  private static GCDResult gcd(final long a, final long b) {
    long s = 0L;
    long oldS = 1L;
    long r = b;
    long oldR = a;
    while (r != 0L) {
      final long quotient = oldR / r;
      final long tmp1 = oldR - quotient * r;
      oldR = r;
      r = tmp1;
      final long tmp2 = oldS - quotient * s;
      oldS = s;
      s = tmp2;
    }
    long bezoutT = 0L;
    if (b != 0L) {
      bezoutT = (oldR - oldS * a) / b;
    }
    return new GCDResult(oldS, bezoutT, oldR);
  }

  private static Optional<BezoutResult> solveBezout(final long a, final long b, final long target) {
    final GCDResult result = gcd(a, b);
    final long lcm = result.gcd() * (a / result.gcd()) * (b / result.gcd());
    final long increment1 = lcm / a;
    final long increment2 = lcm / b;
    if (target % result.gcd() != 0) {
      return Optional.empty();
    }
    final long factor = target / result.gcd();
    long factor1 = factor * result.coeff1();
    long factor2 = factor * result.coeff2();
    if (factor1 < 0) {
      final long steps = Math.abs(factor1 / increment1) + 1;
      factor1 = factor1 + steps * increment1;
      factor2 = factor2 - steps * increment2;
    }
    if (factor2 < 0) {
      final long steps = Math.abs(factor2 / increment2) + 1;
      factor1 = factor1 - steps * increment1;
      factor2 = factor2 + steps * increment2;
    }
    return Optional.of(new BezoutResult(factor1, factor2, increment1, increment2));
  }

  private static long solveMachine(ClawMachine machine) {
    final Optional<BezoutResult> result1 = solveBezout(machine.a()
        .x(), machine.b()
        .x(), machine.prize()
        .x());
    final Optional<BezoutResult> result2 = solveBezout(machine.a()
        .y(), machine.b()
        .y(), machine.prize()
        .y());
    if (result1.isEmpty() || result2.isEmpty()) {
      return 0L;
    }
    long coeff1 = result1.orElseThrow()
        .coeff1();
    long coeff2 = result1.orElseThrow()
        .coeff2();
    long increment1 = result1.orElseThrow()
        .increment1();
    long increment2 = result1.orElseThrow()
        .increment2();
    long steps = 0L;
    if (COST_BUTTON_A * increment1 > COST_BUTTON_B * increment2) {
      steps = coeff1 / increment1;
      increment1 = -increment1;
    } else {
      steps = coeff2 / increment2;
      increment2 = -increment2;
    }
    coeff1 = coeff1 + steps * increment1;
    coeff2 = coeff2 + steps * increment2;
    increment1 = -increment1;
    increment2 = -increment2;
    final long yChange = increment1 * machine.a()
        .y() + increment2 * machine.b()
        .y();
    final long yCurrent = coeff1 * machine.a()
        .y() + coeff2 * machine.b()
        .y();
    long yRest = machine.prize()
        .y() - yCurrent;
    if (yRest * yChange < 0 || yRest % yChange != 0) {
      return 0L;
    }
    coeff1 = coeff1 + (yRest / yChange) * increment1;
    coeff2 = coeff2 + (yRest / yChange) * increment2;
    return COST_BUTTON_A * coeff1 + COST_BUTTON_B * coeff2;
  }

  private static long solve(final ClawMachine[] machines) {
    return Arrays.stream(machines)
        .reduce(0L, (acc, curr) -> acc + solveMachine(curr), Long::sum);
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final ClawMachine[] machines1 = parseInput(input);
    final long result1 = solve(machines1);
    System.out.println("Part 1: " + result1);
    final ClawMachine[] machines2 = Arrays.stream(machines1)
        .map(m ->
            new ClawMachine(
                m.a(),
                m.b(),
                new Prize(
                    m.prize()
                        .x() + OFFSET_PART2,
                    m.prize()
                        .y() + OFFSET_PART2)
            )
        )
        .toArray(ClawMachine[]::new);
    final long result2 = solve(machines2);
    System.out.println("Part 2: " + result2);
  }

  private record BezoutResult(long coeff1, long coeff2, long increment1, long increment2) {

  }

  private record GCDResult(long coeff1, long coeff2, long gcd) {

  }

  private record Prize(long x, long y) {

  }

  private record Button(long x, long y, long cost) {

  }

  private record ClawMachine(Button a, Button b, Prize prize) {

  }
}
