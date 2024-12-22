package org.day21;

public enum Direction {
  LEFT,
  UP,
  RIGHT,
  DOWN,
  ACTIVATE;

  public static final int NUM_DIRECTIONS = 5;

  static {
    LEFT.code = 10;
    UP.code = 11;
    RIGHT.code = 12;
    DOWN.code = 13;
    ACTIVATE.code = 14;
  }

  public int code;

  public static Direction[] getDirections(final Coordinate source, final Coordinate target) {
    final int yDiff = target.y() - source.y();
    final int xDiff = target.x() - source.x();
    if (yDiff == 0 && xDiff == 0) {
      return new Direction[]{};
    }
    if (yDiff == 0) {
      return new Direction[]{
          xDiff < 0 ? LEFT : RIGHT
      };
    }
    if (xDiff == 0) {
      return new Direction[]{
          yDiff < 0 ? UP : DOWN
      };
    }
    return new Direction[]{
        xDiff < 0 ? LEFT : RIGHT,
        yDiff < 0 ? UP : DOWN
    };
  }

  @Override
  public String toString() {
    return switch (this) {
      case LEFT -> "<";
      case UP -> "^";
      case RIGHT -> ">";
      case DOWN -> "v";
      case ACTIVATE -> "A";
    };
  }

  public int[] getDirectionArray() {
    return switch (this) {
      case LEFT -> new int[]{-1, 0};
      case UP -> new int[]{0, -1};
      case RIGHT -> new int[]{1, 0};
      case DOWN -> new int[]{0, 1};
      case ACTIVATE -> new int[]{0, 0};
    };
  }
}
