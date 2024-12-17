package org.day17;

import java.util.Arrays;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day17.txt";

  private Main() {
    super();
  }

  private static long parseRegister(final String input) {
    return Long.parseLong(input.split(": ")[1]);
  }

  private static int[] parseInstructions(final String input) {
    final String instructionsStr = input.split(": ")[1];
    final String[] parts = instructionsStr.split(",");
    return Arrays.stream(parts)
        .mapToInt(Integer::parseInt)
        .toArray();
  }

  private static Program parseInput(final String input) {
    final String[] parts = input.split(System.lineSeparator() + System.lineSeparator());
    final String[] registers = parts[0].split(System.lineSeparator());
    final long registerA = parseRegister(registers[0]);
    final long registerB = parseRegister(registers[1]);
    final long registerC = parseRegister(registers[2]);
    final int[] instructions = parseInstructions(parts[1]);
    return new Program(registerA, registerB, registerC, instructions);
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Program program = parseInput(input);
    final String result1 = program.execute();
    System.out.println("Part 1: " + result1);
  }
}
