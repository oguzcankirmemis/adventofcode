package org.day16;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
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

  private static Coordinate getEnd(final char[][] map) {
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        if (map[i][j] == END) {
          return new Coordinate(j, i);
        }
      }
    }
    return null;
  }

  private static long dfs1(final char[][] map, final Reindeer start) {
    final Map<Reindeer, Long> visited = new HashMap<>();
    final Stack<StackEntry> stack = new Stack<>();
    stack.push(new StackEntry(start, 0L, Set.of()));
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
          stack.push(new StackEntry(n, c + 1L, Set.of()));
        }
        stack.push(new StackEntry(r.clockwise(), c + 1000L, Set.of()));
        stack.push(new StackEntry(r.counterClockwise(), c + 1000L, Set.of()));
      }
    }
    return maxCost;
  }

  private static long dfs2(final char[][] map, final Reindeer start, final long targetCost) {
    final Set<Coordinate> uniqueLocations = new HashSet<>();
    final Map<Reindeer, Long> visited = new HashMap<>();
    final Stack<StackEntry> stack = new Stack<>();
    stack.push(new StackEntry(start, 0L, Set.of(new Coordinate(start.x(), start.y()))));
    while (!stack.isEmpty()) {
      final StackEntry e = stack.pop();
      final Reindeer r = e.deer();
      final long c = e.cost();
      final Set<Coordinate> locations = e.locations();
      if (c > targetCost) {
        continue;
      }
      if (map[r.y()][r.x()] == END) {
        uniqueLocations.addAll(locations);
        continue;
      }
      if (c == targetCost) {
        continue;
      }
      if (c <= visited.getOrDefault(r, targetCost)) {
        visited.put(r, c);
        final Reindeer n = r.move(map);
        if (n != r) {
          Set<Coordinate> nextLocations = new HashSet<>(locations);
          nextLocations.add(new Coordinate(n.x(), n.y()));
          stack.push(new StackEntry(n, c + 1L, nextLocations));
        }
        stack.push(new StackEntry(r.clockwise(), c + 1000L, locations));
        stack.push(new StackEntry(r.counterClockwise(), c + 1000L, locations));
      }
    }
    return uniqueLocations.size();
  }

  @SuppressWarnings("unused")
  private static String toString(final char[][] map, Set<Coordinate> seats) {
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        if (seats.contains(new Coordinate(j, i))) {
          sb.append('O');
        } else {
          sb.append(map[i][j]);
        }
      }
      sb.append(System.lineSeparator());
    }
    return sb.toString();
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final char[][] map = parseInput(input);
    final Reindeer start = getStart(map);
    final Coordinate end = getEnd(map);
    assert end != null;
    final long result1 = dfs1(map, start);
    System.out.println("Part 1: " + result1);
    final long result2 = dfs2(map, start, result1);
    System.out.println("Part 2: " + result2);
  }

  private record StackEntry(Reindeer deer, long cost, Set<Coordinate> locations) {

  }

  private record Coordinate(int x, int y) {

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
