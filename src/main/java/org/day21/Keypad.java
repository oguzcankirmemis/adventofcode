package org.day21;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class Keypad {

  public static final int EMPTY = -1;

  protected final Map<Integer, Coordinate> coords = new HashMap<>();
  protected int[][] keypad;
  protected Coordinate position;

  public boolean isInbounds(final Coordinate coord) {
    return coord.y() >= 0 && coord.y() < keypad.length &&
        coord.x() >= 0 && coord.x() < keypad[coord.y()].length &&
        keypad[coord.y()][coord.x()] != EMPTY;
  }

  public List<List<Direction>> move(
      final List<Direction> current,
      final Coordinate source,
      final Coordinate target
  ) {
    if (source.y() == target.y() && source.x() == target.x()) {
      return List.of(current);
    }
    final Direction[] dirs = Direction.getDirections(source, target);
    final List<List<Direction>> result = new ArrayList<>();
    for (final Direction dir : dirs) {
      final int[] dirArray = dir.getDirectionArray();
      final Coordinate newCoordinate = new Coordinate(
          source.x() + dirArray[0],
          source.y() + dirArray[1]
      );
      if (!isInbounds(newCoordinate)) {
        continue;
      }
      List<Direction> copy = new ArrayList<>(current);
      copy.add(dir);
      result.addAll(move(copy, newCoordinate, target));
    }
    return result;
  }

  public List<List<Direction>> pressButton(final int button) {
    final Coordinate target = coords.get(button);
    assert target != null;
    final List<List<Direction>> result = move(new ArrayList<>(), position, target);
    this.position = target;
    return result;
  }
}
