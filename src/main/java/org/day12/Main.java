package org.day12;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day12.txt";

  private Main() {
    super();
  }

  private static char[][] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(String::toCharArray)
        .toArray(char[][]::new);
  }

  private static Optional<Region> dfs(final char[][] map,
      final Coord point,
      final Set<Coord> visited,
      final HashMap<Coord, HashMap<Integer, ArrayList<HashSet<Integer>>>> sideMap) {
    if (visited.contains(point)) {
      return Optional.empty();
    }
    visited.add(point);
    point.perimeterSides(map)
        .stream()
        .forEach(c -> {
          sideMap.putIfAbsent(c, new HashMap<Integer, ArrayList<HashSet<Integer>>>());
          int key = 0;
          int val = 0;
          if (c.y() == 0) {
            key = point.x();
            val = point.y();
          } else {
            key = point.y();
            val = point.x();
          }
          sideMap.get(c)
              .putIfAbsent(key, new ArrayList<HashSet<Integer>>());
          ArrayList<HashSet<Integer>> sides = sideMap.get(c)
              .get(key);
          ArrayList<HashSet<Integer>> parts = new ArrayList<HashSet<Integer>>();
          for (HashSet<Integer> line : sides) {
            if (line.contains(val - 1) || line.contains(val + 1)) {
              parts.add(line);
            }
          }
          if (parts.size() == 2) {
            parts.get(0)
                .addAll(parts.get(1));
            parts.get(0)
                .add(val);
            sides.remove(parts.get(1));
          } else if (parts.size() == 1) {
            parts.get(0)
                .add(val);
          } else {
            HashSet<Integer> set = new HashSet<Integer>();
            set.add(val);
            sides.add(set);
          }
        });
    int perimeter = point.perimeter(map);
    int area = 1;
    for (Coord neighbour : point.neighbours(map)) {
      if (!visited.contains(neighbour)) {
        final Region region = dfs(map, neighbour, visited, sideMap).orElseThrow();
        perimeter += region.perimeter();
        area += region.area();
      }
    }
    return Optional.of(new Region(perimeter, area));
  }

  private static int sumRegionValues1(final char[][] map) {
    int sum = 0;
    final Set<Coord> visited = new HashSet<Coord>();
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        HashMap<Coord, HashMap<Integer, ArrayList<HashSet<Integer>>>> sidesMap =
            new HashMap<Coord, HashMap<Integer, ArrayList<HashSet<Integer>>>>();
        final Coord point = new Coord(j, i);
        final Region region = dfs(map, point, visited, sidesMap).orElse(new Region(0, 0));
        sum += region.value();
      }
    }
    return sum;
  }

  private static int sumRegionValues2(final char[][] map) {
    int sum = 0;
    final Set<Coord> visited = new HashSet<Coord>();
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        HashMap<Coord, HashMap<Integer, ArrayList<HashSet<Integer>>>> sidesMap =
            new HashMap<Coord, HashMap<Integer, ArrayList<HashSet<Integer>>>>();
        final Coord point = new Coord(j, i);
        final Region region = dfs(map, point, visited, sidesMap).orElse(new Region(0, 0));
        int sides = 0;
        for (Coord c : sidesMap.keySet()) {
          for (Integer key : sidesMap.get(c)
              .keySet()) {
            sides += sidesMap.get(c)
                .get(key)
                .size();
          }
        }
        sum += region.area() * sides;
      }
    }
    return sum;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final char[][] map = parseInput(input);
    int result1 = sumRegionValues1(map);
    System.out.println("Part 1: " + result1);
    int result2 = sumRegionValues2(map);
    System.out.println("Part 2: " + result2);
  }

  private record Region(int perimeter, int area) {

    private int value() {
      return perimeter * area;
    }
  }

  private record Coord(int x, int y) {

    private boolean isInbounds(final char[][] map) {
      return y >= 0 && y < map.length && x >= 0 && x < map[y].length;
    }

    private List<Coord> neighbours(final char[][] map) {
      final Coord[] dirs = new Coord[]{
          new Coord(0, 1),
          new Coord(0, -1),
          new Coord(1, 0),
          new Coord(-1, 0)
      };
      return Arrays.stream(dirs)
          .map(d -> new Coord(x + d.x(), y + d.y()))
          .filter(c -> c.isInbounds(map))
          .filter(c -> map[y][x] == map[c.y()][c.x()])
          .toList();
    }

    private List<Coord> perimeterSides(final char[][] map) {
      final Coord[] dirs = new Coord[]{
          new Coord(0, 1),
          new Coord(0, -1),
          new Coord(1, 0),
          new Coord(-1, 0)
      };
      return Arrays.stream(dirs)
          .filter(c -> {
            final Coord next = new Coord(x + c.x(), y + c.y());
            if (!next.isInbounds(map)) {
              return true;
            }
            return map[y][x] != map[next.y()][next.x()];
          })
          .toList();
    }

    private int perimeter(final char[][] map) {
      return 4 - neighbours(map).size();
    }
  }
}
