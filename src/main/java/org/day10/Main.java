package org.day10;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day10.txt";

  private Main() {
    super();
  }

  private static int[][] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(String::chars)
        .map(line ->
            line.map(c -> Integer.parseInt("" + (char) c))
                .toArray()
        )
        .toArray(int[][]::new);
  }

  private static void dfs1(final Coordinate curr,
      final int[][] map,
      final Set<Coordinate> visited,
      final Set<Coordinate> reached) {
    visited.add(curr);
    if (curr.isEnd(map)) {
      reached.add(curr);
      return;
    }
    for (Coordinate candidate : curr.next(map)) {
      if (!visited.contains(candidate)) {
        dfs1(candidate, map, visited, reached);
      }
    }
  }

  private static int dfs2(final Coordinate curr, final int[][] map) {
    if (curr.isEnd(map)) {
      return 1;
    }
    int rating = 0;
    for (Coordinate candidate : curr.next(map)) {
      rating += dfs2(candidate, map);
    }
    return rating;
  }

  private static int calculateScore(final int[][] map) {
    int score = 0;
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        final Coordinate coord = new Coordinate(j, i);
        if (coord.isStart(map)) {
          Set<Coordinate> visited = new HashSet<Coordinate>();
          Set<Coordinate> reached = new HashSet<Coordinate>();
          dfs1(coord, map, visited, reached);
          score += reached.size();
        }
      }
    }
    return score;
  }

  private static int calculateRating(final int[][] map) {
    int rating = 0;
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        final Coordinate coord = new Coordinate(j, i);
        if (coord.isStart(map)) {
          rating += dfs2(coord, map);
        }
      }
    }
    return rating;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final int[][] map = parseInput(input);
    final int result1 = calculateScore(map);
    System.out.println("Part 1: " + result1);
    final int result2 = calculateRating(map);
    System.out.println("Part 2: " + result2);
  }

  private record Coordinate(int x, int y) {

    private boolean isInbounds(final int[][] map) {
      return 0 <= y && y < map.length && 0 <= x && x < map[y].length;
    }

    private boolean isStart(final int[][] map) {
      return map[y][x] == 0;
    }

    private boolean isEnd(final int[][] map) {
      return map[y][x] == 9;
    }

    private List<Coordinate> next(final int[][] map) {
      final Coordinate[] dirs = new Coordinate[]{
          new Coordinate(1, 0),
          new Coordinate(-1, 0),
          new Coordinate(0, 1),
          new Coordinate(0, -1)
      };
      final List<Coordinate> next = new ArrayList<Coordinate>(4);
      for (final Coordinate dir : dirs) {
        final Coordinate candidate = new Coordinate(x + dir.x(), y + dir.y());
        if (candidate.isInbounds(map) && map[candidate.y()][candidate.x()] - map[y][x] == 1) {
          next.add(candidate);
        }
      }
      return next;
    }
  }
}
