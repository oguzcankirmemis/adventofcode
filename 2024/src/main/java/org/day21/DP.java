package org.day21;

import java.util.List;

public class DP {

  private static List<List<Instruction>> getInstructions(
      final DirectionalKeypad keypad,
      final Coordinate source,
      final Coordinate target
  ) {
    final int xDiff = target.x() - source.x();
    final int yDiff = target.y() - source.y();
    final Direction xDir = xDiff < 0 ? Direction.LEFT : Direction.RIGHT;
    final Direction yDir = yDiff < 0 ? Direction.UP : Direction.DOWN;
    final Instruction xInstr = new Instruction(xDir, Math.abs(xDiff));
    final Instruction yInstr = new Instruction(yDir, Math.abs(yDiff));
    if (xDiff == 0 && yDiff == 0) {
      return List.of();
    }
    if (xDiff == 0) {
      return List.of(List.of(yInstr));
    }
    if (yDiff == 0) {
      return List.of(List.of(xInstr));
    }
    final Coordinate empty = keypad.coords.get(Keypad.EMPTY);
    if (empty.x() == source.x() && empty.y() == yDiff + source.y()) {
      return List.of(List.of(xInstr, yInstr));
    }
    if (empty.y() == source.y() && empty.x() == xDiff + source.x()) {
      return List.of(List.of(yInstr, xInstr));
    }
    return List.of(
        List.of(xInstr, yInstr),
        List.of(yInstr, xInstr)
    );
  }

  public static long[][][] dp(final DirectionalKeypad keypad, final int numOfIterations) {
    final long[][][] table =
        new long[numOfIterations + 1][Direction.NUM_DIRECTIONS][Direction.NUM_DIRECTIONS];
    for (int i = 0; i < Direction.NUM_DIRECTIONS; i++) {
      for (int j = 0; j < Direction.NUM_DIRECTIONS; j++) {
        table[0][i][j] = 1L;
      }
    }
    for (int step = 1; step <= numOfIterations; step++) {
      for (final Direction dir1 : Direction.values()) {
        for (final Direction dir2 : Direction.values()) {
          final int key1 = dir1.code % Direction.NUM_DIRECTIONS;
          final int key2 = dir2.code % Direction.NUM_DIRECTIONS;
          table[step][key1][key2] = Long.MAX_VALUE;
          final Coordinate source = keypad.coords.get(dir1.code);
          final Coordinate target = keypad.coords.get(dir2.code);
          final List<List<Instruction>> instructionsList = getInstructions(keypad, source, target);
          if (instructionsList.isEmpty()) {
            table[step][key1][key2] = table[step - 1][key1][key2];
            continue;
          }
          final int activateButtonKey = Direction.ACTIVATE.code % Direction.NUM_DIRECTIONS;
          for (final List<Instruction> instructions : getInstructions(keypad, source, target)) {
            long sub = 0;
            int currKey = activateButtonKey;
            for (final Instruction i : instructions) {
              final int key3 = i.dir().code % Direction.NUM_DIRECTIONS;
              sub += table[step - 1][currKey][key3] + i.amount() - 1;
              currKey = key3;
            }
            sub += table[step - 1][currKey][activateButtonKey];
            table[step][key1][key2] = Math.min(table[step][key1][key2], sub);
          }
        }
      }
    }
    return table;
  }

  private record Instruction(Direction dir, long amount) {

  }
}
