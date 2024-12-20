package org.day20;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day20.txt";

  private static final char WALL = '#';
  private static final char START = 'S';
  private static final char END = 'E';

  private Main() {
    super();
  }

  private static char[][] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(String::toCharArray)
        .toArray(char[][]::new);
  }

  private static Position getStart(final char[][] map) {
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        final Position p = new Position(j, i);
        if (p.isStart(map)) {
          return p;
        }
      }
    }
    return null;
  }

  private static Position getEnd(final char[][] map) {
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        final Position p = new Position(j, i);
        if (p.isEnd(map)) {
          return p;
        }
      }
    }
    return null;
  }

  private static Position next(final char[][] map, final Position prev, final Position curr) {
    final int[][] dirs = new int[][]{
        {0, 1},
        {1, 0},
        {0, -1},
        {-1, 0}
    };
    return Arrays.stream(dirs)
        .map(d -> new Position(curr.x() + d[0], curr.y() + d[1]))
        .filter(p -> p.isEmpty(map) && !p.equals(prev))
        .findFirst()
        .orElseThrow();
  }

  private static Cheat[] getCheats(final char[][] map, final Position position, final int maxDistance) {
    final List<Position> endPositions = new ArrayList<>();
    for (int i = 1; i <= maxDistance; i++) {
      for (int j = 0; j <= i; j++) {
        final List<Position> possibleEndPositions = List.of(
            new Position(position.x() + j, position.y() + i - j),
            new Position(position.x() + j, position.y() - i + j),
            new Position(position.x() - j, position.y() + i - j),
            new Position(position.x() - j, position.y() - i + j)
        );
        endPositions.addAll(possibleEndPositions);
      }
    }
    return endPositions.stream()
        .filter(p -> p.isEmpty(map))
        .map(end -> new Cheat(position, end))
        .toArray(Cheat[]::new);
  }

  public static int[][] getLengthMap(final char[][] map) {
    final int[][] lengthMap = new int[map.length][map[0].length];
    final Position start = getStart(map);
    assert start != null;
    final Position end = getEnd(map);
    assert end != null;
    lengthMap[end.y()][end.x()] = 0;
    Position prevPosition = null;
    Position currPosition = end;
    int curr = 1;
    while (!currPosition.equals(start)) {
      final Position tmp = currPosition;
      currPosition = next(map, prevPosition, currPosition);
      prevPosition = tmp;
      lengthMap[currPosition.y()][currPosition.x()] = curr;
      curr++;
    }
    return lengthMap;
  }

  private static int countGoodCheats(final char[][] map, final int shortcutThreshold, final int maxDistance) {
    Set<Cheat> goodCheats = new HashSet<>();
    final int[][] lengthMap = getLengthMap(map);
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        final Position p = new Position(j, i);
        if (p.isEmpty(map)) {
          goodCheats.addAll(Arrays.stream(getCheats(map, p, maxDistance))
              .filter(c -> {
                final Position s = c.source();
                final Position d = c.destination();
                final int distance = Math.abs(d.x() - s.x()) + Math.abs(d.y() - s.y());
                return lengthMap[p.y()][p.x()] - lengthMap[d.y()][d.x()] - distance >= shortcutThreshold;
              })
              .toList()
          );
        }
      }
    }
    return goodCheats.size();
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final char[][] map = parseInput(input);
    final int result1 = countGoodCheats(map, 100, 2);
    System.out.println("Part 1: " + result1);
    final int result2 = countGoodCheats(map, 100, 20);
    System.out.println("Part 2: " + result2);
  }

  private record Cheat(Position source, Position destination) {

  }

  private record Position(int x, int y) {

    private boolean isInbounds(final char[][] map) {
      return y >= 0 && y < map.length && x >= 0 && x < map[y].length;
    }

    private boolean isStart(final char[][] map) {
      return isInbounds(map) && map[y][x] == START;
    }

    private boolean isEnd(final char[][] map) {
      return isInbounds(map) && map[y][x] == END;
    }

    private boolean isEmpty(final char[][] map) {
      return isInbounds(map) && map[y][x] != WALL;
    }
  }
}
