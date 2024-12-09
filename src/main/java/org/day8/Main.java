package org.day8;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day8.txt";

  private Main() {
    super();
  }

  private static char[][] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(String::toCharArray)
        .toArray(char[][]::new);
  }

  private static Map<Character, List<Coordinate>> frequencyMap(char[][] map) {
    final Map<Character, List<Coordinate>> antennaMap = new HashMap<Character, List<Coordinate>>();
    for (int i = 0; i < map.length; i++) {
      for (int j = 0; j < map[i].length; j++) {
        if ('.' != map[i][j]) {
          antennaMap.putIfAbsent(map[i][j], new ArrayList<Coordinate>());
          antennaMap.get(map[i][j])
              .add(new Coordinate(j, i));
        }
      }
    }
    return antennaMap;
  }

  private static int countAntinodes1(final char[][] map) {
    final Set<Coordinate> antinodes = new HashSet<Coordinate>();
    final Map<Character, List<Coordinate>> antennaMap = Main.frequencyMap(map);
    for (final List<Coordinate> freqList : antennaMap.values()) {
      for (int i = 0; i < freqList.size(); i++) {
        final Coordinate coord1 = freqList.get(i);
        for (int j = i + 1; j < freqList.size(); j++) {
          final Coordinate coord2 = freqList.get(j);
          for (final Coordinate antinode : coord1.antinodes1(coord2)) {
            if (antinode.isInbounds(map)) {
              antinodes.add(antinode);
            }
          }
        }
      }
    }
    return antinodes.size();
  }

  private static int countAntinodes2(final char[][] map) {
    final Set<Coordinate> antinodes = new HashSet<Coordinate>();
    final Map<Character, List<Coordinate>> antennaMap = Main.frequencyMap(map);
    for (final List<Coordinate> freqList : antennaMap.values()) {
      for (int i = 0; i < freqList.size(); i++) {
        final Coordinate coord1 = freqList.get(i);
        for (int j = i + 1; j < freqList.size(); j++) {
          final Coordinate coord2 = freqList.get(j);
          antinodes.addAll(coord1.antinodes2(coord2, map));
        }
      }
    }
    return antinodes.size();
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(Main.inputFile);
    final char[][] map = Main.parseInput(input);
    final int result1 = Main.countAntinodes1(map);
    System.out.println("Part 1: " + result1);
    final int result2 = Main.countAntinodes2(map);
    System.out.println("Part 2: " + result2);
  }

  private record Coordinate(int x, int y) {

    private Coordinate[] antinodes1(final Coordinate coord) {
      final int xDiff = this.x() - coord.x();
      final int yDiff = this.y() - coord.y();
      final Coordinate antinode1 = new Coordinate(this.x() + xDiff, this.y() + yDiff);
      final Coordinate antinode2 = new Coordinate(coord.x() - xDiff, coord.y() - yDiff);
      return new Coordinate[]{antinode1, antinode2};
    }

    private List<Coordinate> antinodes2(final Coordinate coord, final char[][] map) {
      final List<Coordinate> antinodes = new ArrayList<Coordinate>();
      antinodes.add(this);
      antinodes.add(coord);
      final int xDiff = this.x() - coord.x();
      final int yDiff = this.y() - coord.y();
      Coordinate antinode1 = new Coordinate(this.x() + xDiff, this.y() + yDiff);
      while (antinode1.isInbounds(map)) {
        antinodes.add(antinode1);
        antinode1 = new Coordinate(antinode1.x() + xDiff, antinode1.y() + yDiff);
      }
      Coordinate antinode2 = new Coordinate(coord.x() - xDiff, coord.y() - yDiff);
      while (antinode2.isInbounds(map)) {
        antinodes.add(antinode2);
        antinode2 = new Coordinate(antinode2.x() - xDiff, antinode2.y() - yDiff);
      }
      return antinodes;
    }

    private boolean isInbounds(final char[][] map) {
      return 0 <= this.y() && this.y < map.length && 0 <= this.x() && this.x() < map[this.y()].length;
    }
  }
}
