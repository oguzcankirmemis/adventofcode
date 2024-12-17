package org.day17;

import java.util.ArrayList;
import java.util.List;

public class Program {

  private static final int ADV = 0;
  private static final int BXL = 1;
  private static final int BST = 2;
  private static final int JNZ = 3;
  private static final int BXC = 4;
  private static final int OUT = 5;
  private static final int BDV = 6;
  private static final int CDV = 7;

  private final int[] instructions;
  private final List<Long> output;
  private long a;
  private long b;
  private long c;
  private int ip;


  public Program(final long a, final long b, final long c, final int[] instructions) {
    this.a = a;
    this.b = b;
    this.c = c;
    this.ip = 0;
    this.instructions = instructions;
    this.output = new ArrayList<>();
  }

  private long getComboOperand(int operand) {
    return switch (operand) {
      case 6 -> c;
      case 5 -> b;
      case 4 -> a;
      default -> operand;
    };
  }

  private void executeInstruction(final Instruction i) {
    switch (i.instruction()) {
      case ADV:
        a = a / (1L << getComboOperand(i.operand()));
        ip += 2;
        return;
      case BXL:
        b = b ^ (long) i.operand();
        ip += 2;
        return;
      case BST:
        b = getComboOperand(i.operand()) % 8;
        ip += 2;
        return;
      case JNZ:
        if (a == 0) {
          ip += 2;
          return;
        }
        ip = i.operand();
        return;
      case BXC:
        b = b ^ c;
        ip += 2;
        return;
      case OUT:
        output.add(getComboOperand(i.operand()) % 8);
        ip += 2;
        return;
      case BDV:
        b = a / (1L << getComboOperand(i.operand()));
        ip += 2;
        return;
      case CDV:
        c = a / (1L << getComboOperand(i.operand()));
        ip += 2;
        return;
      default:
    }
  }

  public String execute() {
    while (0 <= ip && ip + 1 < instructions.length) {
      final Instruction i = new Instruction(instructions[ip], instructions[ip + 1]);
      executeInstruction(i);
    }
    return output.stream()
        .reduce("", (acc, curr) -> acc + "," + curr, (String::concat))
        .substring(1);
  }

  public record Instruction(int instruction, int operand) {

  }
}
