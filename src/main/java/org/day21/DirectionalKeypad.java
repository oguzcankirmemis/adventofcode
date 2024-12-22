package org.day21;

import java.util.ArrayList;
import java.util.List;

public class DirectionalKeypad extends Keypad {

  public DirectionalKeypad() {
    super();
    this.keypad = new int[][]{
        {EMPTY, Direction.UP.code, Direction.ACTIVATE.code},
        {Direction.LEFT.code, Direction.DOWN.code, Direction.RIGHT.code}
    };
    for (int i = 0; i < keypad.length; i++) {
      for (int j = 0; j < keypad[i].length; j++) {
        this.coords.put(keypad[i][j], new Coordinate(j, i));
      }
    }
    this.position = this.coords.get(Direction.ACTIVATE.code);
  }

  public List<List<Direction>> pressCode(final List<Direction> code) {
    List<List<Direction>> result = new ArrayList<>();
    result.add(List.of());
    for (final Direction d : code) {
      List<List<Direction>> newResult = new ArrayList<>();
      final List<List<Direction>> subResult = pressButton(d.code);
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

  public List<List<Direction>> pressCodeShortest(final List<List<Direction>> codes) {
    int length = Integer.MAX_VALUE;
    List<List<Direction>> result = new ArrayList<>();
    for (final List<Direction> code : codes) {
      List<List<Direction>> subResult = pressCode(code);
      if (subResult.getFirst().size() < length) {
        result = subResult;
        length = subResult.getFirst().size();
      } else if (subResult.getFirst().size() == length) {
        result.addAll(subResult);
      }
    }
    return result;
  }
}
