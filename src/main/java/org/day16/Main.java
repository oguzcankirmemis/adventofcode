package org.day16;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Stack;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day16.txt";

  private static final char START = 'S';
  private static final char END = 'E';
  private static final char WALL = '#';
  private static final int[][] DIRS = {
      {1, 0},
      {0, 1},
      {-1, 0},
      {0, -1}
  };

  private Main() {
    super();
  }

  private static char[][] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(String::toCharArray)
        .toArray(char[][]::new);
  }

  private static Reindeer getStart(final char[][] map) {
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        if (map[i][j] == START) {
          return new Reindeer(j, i, 0);
        }
      }
    }
    return null;
  }

  private static long dfs(final char[][] map, final Reindeer start) {
    final Map<Reindeer, Long> visited = new HashMap<>();
    final Stack<StackEntry> stack = new Stack<>();
    stack.push(new StackEntry(start, 0L));
    long maxCost = Long.MAX_VALUE;
    while (!stack.isEmpty()) {
      final StackEntry e = stack.pop();
      final Reindeer r = e.deer();
      final long c = e.cost();
      if (c >= maxCost) {
        continue;
      }
      if (map[r.y()][r.x()] == END) {
        maxCost = c;
        continue;
      }
      if (c < visited.getOrDefault(r, maxCost)) {
        visited.put(r, c);
        final Reindeer n = r.move(map);
        if (n != r) {
          stack.push(new StackEntry(n, c + 1L));
        }
        stack.push(new StackEntry(r.clockwise(), c + 1000L));
        stack.push(new StackEntry(r.counterClockwise(), c + 1000L));
      }
    }
    return maxCost;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final char[][] map = parseInput(input);
    final Reindeer start = getStart(map);
    final long result1 = dfs(map, start);
    System.out.println("Part 1: " + result1);
  }

  private record StackEntry(Reindeer deer, long cost) {

  }

  private record Reindeer(int x, int y, int dir) {

    private Reindeer clockwise() {
      final int newDir = (dir + 1) % DIRS.length;
      return new Reindeer(x, y, newDir);
    }

    private Reindeer counterClockwise() {
      int newDir = dir - 1;
      if (newDir < 0) {
        newDir += DIRS.length;
      }
      return new Reindeer(x, y, newDir);
    }

    private Reindeer move(final char[][] map) {
      final int newX = x + DIRS[dir][0];
      final int newY = y + DIRS[dir][1];
      if (newY < 0 || newY >= map.length || newX < 0 || newX > map[newY].length ||
          map[newY][newX] == WALL) {
        return this;
      }
      return new Reindeer(newX, newY, dir);
    }
  }
}
