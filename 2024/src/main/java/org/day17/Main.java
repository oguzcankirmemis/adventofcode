package org.day17;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
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


  /**
   * Input Program: BST A; BXL 7; CDV B; ADV 3; BXL 7; BXC; OUT B; JNZ 0;
   */
  private static String executeProgramLogic(long registerA) {
    StringBuilder result = new StringBuilder();
    while (registerA != 0L) {
      final long modA = registerA % 8;
      result.append(",")
          .append((modA ^ (registerA / (1L << ((registerA % 8) ^ 7)))) % 8);
      registerA = registerA / 8;
    }
    return result.substring(1);
  }

  /**
   * Input Program: BST A; BXL 7; CDV B; ADV 3; BXL 7; BXC; OUT B;
   */
  private static long executeProgramIteration(final long registerA) {
    final long modA = registerA % 8;
    return (modA ^ (registerA / (1L << ((registerA % 8) ^ 7)))) % 8;
  }

  private static long findRegisterA(final Program program, final long registerStart, final int index) {
    if (index < 0) {
      return registerStart >> 3;
    }
    long input = registerStart;
    List<Long> candidates = new ArrayList<>();
    for (int j = 0; j < 7; j++) {
      if (executeProgramIteration(input) == program.getInstruction(index)) {
        candidates.add(input << 3);
      }
      input++;
    }
    final String instructionsStr = program.getInstructionsString();
    for (long candidate : candidates) {
      long result = findRegisterA(program, candidate, index - 1);
      if (executeProgramLogic(result).equals(instructionsStr)) {
        return result;
      }
    }
    return -1L;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Program program = parseInput(input);
    final String result1 = program.execute();
    System.out.println("Part 1: " + result1);
    final long result2 = findRegisterA(program, 1L, program.getInstructionsLength() - 1);
    System.out.println("Part 2: " + result2);
  }
}
