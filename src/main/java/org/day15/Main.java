package org.day15;

import java.util.Arrays;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day15.txt";

  private static final char BOX_LEFT = '[';
  private static final char BOX_RIGHT = ']';
  private static final char WALL = '#';
  private static final char EMPTY = '.';
  private static final char BOX = 'O';
  private static final char ROBOT = '@';

  private static final char UP = '^';
  private static final char LEFT = '<';
  private static final char DOWN = 'v';
  private static final char RIGHT = '>';

  private Main() {
    super();
  }

  private static int[] parseInstructions(final String instructions) {
    return instructions.chars()
        .filter(c -> !System.lineSeparator().contains("" + (char) c))
        .toArray();
  }

  private static char[][] parseMap(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(String::toCharArray)
        .toArray(char[][]::new);
  }

  private static ParseResult parseInput(final String input) {
    final String[] parts = input.split(System.lineSeparator() + System.lineSeparator());
    final char[][] map = parseMap(parts[0]);
    final int[] instructions = parseInstructions(parts[1]);
    return new ParseResult(map, instructions);
  }

  private static char[][] expandMap(final char[][] map) {
    final char[][] expanded = new char[map.length][2 * map[0].length];
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map.length; j++) {
        if (map[i][j] == WALL) {
          expanded[i][2 * j] = WALL;
          expanded[i][2 * j + 1] = WALL;
        }
        if (map[i][j] == BOX) {
          expanded[i][2 * j] = BOX_LEFT;
          expanded[i][2 * j + 1] = BOX_RIGHT;
        }
        if (map[i][j] == EMPTY) {
          expanded[i][2 * j] = EMPTY;
          expanded[i][2 * j + 1] = EMPTY;
        }
        if (map[i][j] == ROBOT) {
          expanded[i][2 * j] = ROBOT;
          expanded[i][2 * j + 1] = EMPTY;
        }
      }
    }
    return expanded;
  }

  private static boolean isInbounds(final int x, final int y, final char[][] map) {
    return 0 <= y && y < map.length && 0 <= x && x < map[y].length;
  }

  private static boolean isWall(final int x, final int y, final char[][] map) {
    if (!isInbounds(x, y, map)) {
      return false;
    }
    return map[y][x] == WALL;
  }

  private static boolean isEmpty(final int x, final int y, final char[][] map) {
    if (!isInbounds(x, y, map)) {
      return false;
    }
    return map[y][x] == EMPTY;
  }

  private static Robot getRobot(final char[][] map) {
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        if (map[i][j] == ROBOT) {
          return new Robot(j, i);
        }
      }
    }
    return null;
  }

  private static void push(
      final char[][] map,
      final int dirX,
      final int dirY,
      final int x,
      final int y
  ) {
    int currX = x;
    int currY = y;
    while (isInbounds(currX, currY, map) && map[currY][currX] == BOX) {
      currX += dirX;
      currY += dirY;
    }
    if (!isEmpty(currX, currY, map)) {
      return;
    }
    while (currX != x || currY != y) {
      map[currY][currX] = BOX;
      currX -= dirX;
      currY -= dirY;
    }
    map[y][x] = EMPTY;
  }

  private static boolean isPushable(
      final char[][] map,
      final int dirX,
      final int dirY,
      final int x,
      final int y
  ) {
    if (map[y][x] != BOX_LEFT && map[y][x] != BOX_RIGHT) {
      return false;
    }
    int nextLeftX = x + dirX;
    int nextRightX = nextLeftX + 1;
    final int nextY = y + dirY;
    if (map[y][x] == BOX_RIGHT) {
      nextRightX = x + dirX;
      nextLeftX = nextRightX - 1;
    }
    if (isWall(nextLeftX, nextY, map) || isWall(nextRightX, nextY, map)) {
      return false;
    }
    if (dirX == -1) {
      if (isEmpty(nextLeftX, nextY, map)) {
        return true;
      }
      return isPushable(map, dirX, dirY, nextLeftX, nextY);
    }
    if (dirX == 1) {
      if (isEmpty(nextRightX, nextY, map)) {
        return true;
      }
      return isPushable(map, dirX, dirY, nextRightX, nextY);
    }
    if (isEmpty(nextLeftX, nextY, map) && isEmpty(nextRightX, nextY, map)) {
      return true;
    }
    return (isEmpty(nextLeftX, nextY, map) || isPushable(map, dirX, dirY, nextLeftX, nextY)) &&
        (isEmpty(nextRightX, nextY, map) || isPushable(map, dirX, dirY, nextRightX, nextY));
  }

  private static void pushExpandedHelper(
      final char[][] map,
      final int dirX,
      final int dirY,
      final int x,
      final int y
  ) {
    if (map[y][x] != BOX_LEFT && map[y][x] != BOX_RIGHT) {
      return;
    }
    int nextLeftX = x + dirX;
    int nextRightX = nextLeftX + 1;
    final int nextY = y + dirY;
    if (map[y][x] == BOX_RIGHT) {
      nextRightX = x + dirX;
      nextLeftX = nextRightX - 1;
    }
    if (dirX == -1) {
      pushExpandedHelper(map, dirX, dirY, nextLeftX, nextY);
    } else if (dirX == 1) {
      pushExpandedHelper(map, dirX, dirY, nextRightX, nextY);
    } else {
      pushExpandedHelper(map, dirX, dirY, nextLeftX, nextY);
      pushExpandedHelper(map, dirX, dirY, nextRightX, nextY);
    }
    map[nextY - dirY][nextLeftX - dirX] = EMPTY;
    map[nextY - dirY][nextRightX - dirX] = EMPTY;
    map[nextY][nextLeftX] = BOX_LEFT;
    map[nextY][nextRightX] = BOX_RIGHT;
  }

  private static void pushExpanded(
      final char[][] map,
      final int dirX,
      final int dirY,
      final int x,
      final int y
  ) {
    if (map[y][x] != BOX_LEFT && map[y][x] != BOX_RIGHT) {
      return;
    }
    if (dirX == -1 && !isPushable(map, dirX, dirY, x, y)) {
      return;
    }
    if (dirX == 1 && !isPushable(map, dirX, dirY, x, y)) {
      return;
    }
    if (!isPushable(map, dirX, dirY, x, y) ||
        !isPushable(map, dirX, dirY, x, y)) {
      return;
    }
    pushExpandedHelper(map, dirX, dirY, x, y);
  }

  private static void simulate(final char[][] map, final int[] instructions) {
    Robot curr = getRobot(map);
    for (int instruction : instructions) {
      assert curr != null;
      curr = curr.simulate(instruction, map);
    }
  }

  private static void simulateExpanded(final char[][] map, final int[] instructions) {
    Robot curr = getRobot(map);
    for (int instruction : instructions) {
      assert curr != null;
      curr = curr.simulateExpanded(instruction, map);
    }
  }

  private static int sumCoordinates(final char[][] map) {
    int sum = 0;
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        if (map[i][j] == BOX) {
          sum += 100 * i + j;
        }
      }
    }
    return sum;
  }

  private static int sumCoordinatesExpanded(final char[][] map) {
    int sum = 0;
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        if (map[i][j] == BOX_LEFT) {
          sum += 100 * i + j;
        }
      }
    }
    return sum;
  }

  @SuppressWarnings("unused")
  private static String toString(final char[][] map) {
    final StringBuilder sb = new StringBuilder();
    for (char[] row : map) {
      for (char tile : row) {
        sb.append(tile);
      }
      sb.append(System.lineSeparator());
    }
    return sb.toString();
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final ParseResult parseResult = parseInput(input);
    final char[][] map = parseResult.map();
    final char[][] expandedMap = expandMap(map);
    final int[] instructions = parseResult.instructions();
    simulate(map, instructions);
    final int result1 = sumCoordinates(map);
    System.out.println("Part 1: " + result1);
    simulateExpanded(expandedMap, instructions);
    final int result2 = sumCoordinatesExpanded(expandedMap);
    System.out.println("Part 2: " + result2);
  }

  private record Robot(int x, int y) {

    private Robot simulate(int instruction, char[][] map) {
      final int[][] dirs = new int[][]{
          new int[]{0, -1},
          new int[]{-1, 0},
          new int[]{0, 1},
          new int[]{1, 0}
      };
      int[] dir = dirs[0];
      switch (instruction) {
        case UP:
          dir = dirs[0];
          break;
        case LEFT:
          dir = dirs[1];
          break;
        case DOWN:
          dir = dirs[2];
          break;
        case RIGHT:
          dir = dirs[3];
          break;
        default:
      }
      final int nextX = x + dir[0];
      final int nextY = y + dir[1];
      push(map, dir[0], dir[1], nextX, nextY);
      if (isEmpty(nextX, nextY, map)) {
        map[y][x] = EMPTY;
        map[nextY][nextX] = ROBOT;
        return new Robot(nextX, nextY);
      }
      return this;
    }

    private Robot simulateExpanded(int instruction, char[][] map) {
      final int[][] dirs = new int[][]{
          new int[]{0, -1},
          new int[]{-1, 0},
          new int[]{0, 1},
          new int[]{1, 0}
      };
      int[] dir = dirs[0];
      switch (instruction) {
        case UP:
          dir = dirs[0];
          break;
        case LEFT:
          dir = dirs[1];
          break;
        case DOWN:
          dir = dirs[2];
          break;
        case RIGHT:
          dir = dirs[3];
          break;
        default:
      }
      final int nextX = x + dir[0];
      final int nextY = y + dir[1];
      pushExpanded(map, dir[0], dir[1], nextX, nextY);
      if (isEmpty(nextX, nextY, map)) {
        map[y][x] = EMPTY;
        map[nextY][nextX] = ROBOT;
        return new Robot(nextX, nextY);
      }
      return this;
    }
  }

  private record ParseResult(char[][] map, int[] instructions) {

  }
}
