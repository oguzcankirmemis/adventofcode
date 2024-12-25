package org.day21;

import java.util.ArrayList;
import java.util.List;

public class NumericKeypad extends Keypad {

  public NumericKeypad() {
    super();
    this.keypad = new int[][]{
        {7, 8, 9},
        {4, 5, 6},
        {1, 2, 3},
        {EMPTY, 0, Direction.ACTIVATE.code}
    };
    for (int i = 0; i < keypad.length; i++) {
      for (int j = 0; j < keypad[i].length; j++) {
        this.coords.put(keypad[i][j], new Coordinate(j, i));
      }
    }
    this.position = this.coords.get(Direction.ACTIVATE.code);
  }

  public List<List<Direction>> pressCode(final String code) {
    List<List<Direction>> result = new ArrayList<>();
    result.add(List.of());
    for (int i = 0; i < code.length(); i++) {
      List<List<Direction>> newResult = new ArrayList<>();
      final char c = code.charAt(i);
      int button = Character.isDigit(c) ? Character.digit(c, 10) : Direction.ACTIVATE.code;
      final List<List<Direction>> subResult = pressButton(button);
      for (final List<Direction> path : subResult) {
        path.add(Direction.ACTIVATE);
        for (final List<Direction> currPath : result) {
          final List<Direction> copy = new ArrayList<>(currPath);
          copy.addAll(path);
          newResult.add(copy);
        }
      }
      result = newResult;
    }
    return result;
  }
}
