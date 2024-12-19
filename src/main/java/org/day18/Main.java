package org.day18;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day18.txt";
  private static final int memorySpace = 71;
  private static final int timestep = 1024;

  private Main() {
    super();
  }

  private static Position[] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(l -> {
          int[] position = Arrays.stream(l.split(","))
              .mapToInt(Integer::parseInt)
              .toArray();
          return new Position(position[0], position[1]);
        })
        .toArray(Position[]::new);
  }

  private static Set<Position> simulateBytes(final Position[] bytes, final int timestep) {
    return Set.copyOf(Arrays.asList(Arrays.copyOfRange(bytes, 0, timestep)));
  }

  private static Position[] neighbours(final Set<Position> bytes, final Position vertex) {
    final int[][] dirs = new int[][]{
        {0, 1},
        {1, 0},
        {0, -1},
        {-1, 0}
    };
    return Arrays.stream(dirs)
        .map(dir -> new Position(vertex.x() + dir[0], vertex.y() + dir[1]))
        .filter(v -> v.x() >= 0 && v.y() >= 0)
        .filter(v -> v.x() < memorySpace && v.y() < memorySpace)
        .filter(v -> !bytes.contains(v))
        .toArray(Position[]::new);
  }

  private static int djikstra(final Set<Position> bytes) {
    final Position source = new Position(0, 0);
    final Position destination = new Position(memorySpace - 1, memorySpace - 1);
    final Map<Position, Integer> distance = new HashMap<>();
    final Map<Position, Position> previous = new HashMap<>();
    final PriorityQueue<Position> queue = new PriorityQueue<>(
        Comparator.comparingInt(p -> distance.getOrDefault(p, Integer.MAX_VALUE))
    );
    queue.add(source);
    while (!queue.isEmpty()) {
      final Position u = queue.poll();
      if (u == destination) {
        break;
      }
      for (final Position v : neighbours(bytes, u)) {
        final int uDistance = distance.getOrDefault(u, Integer.MAX_VALUE);
        final int vDistance = distance.getOrDefault(v, Integer.MAX_VALUE);
        final int alternateDistance = uDistance + 1;
        if (alternateDistance < vDistance) {
          previous.put(v, u);
          distance.put(v, alternateDistance);
          queue.add(v);
        }
      }
    }
    if (!previous.containsKey(destination)) {
      return -1;
    }
    int steps = 0;
    Position curr = destination;
    while (curr != source) {
      steps++;
      curr = previous.get(curr);
    }
    return steps;
  }

  private static Position firstCutoffByte(final Position[] bytes) {
    final Map<Position, Integer> indexes = new HashMap<>();
    for (int i = 0; i < bytes.length; i++) {
      indexes.put(bytes[i], i);
    }
    // binary search returns: (-(insertion_point) - 1)
    final int cutoffIndex = -Arrays.binarySearch(bytes, null, (p, _) -> {
      final Set<Position> fallenBytes = simulateBytes(bytes, indexes.get(p));
      final int searchResult = djikstra(fallenBytes);
      if (searchResult > 0) {
        return -1;
      }
      return indexes.get(p);
    }) - 2;
    return bytes[cutoffIndex];
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Position[] bytes = parseInput(input);
    final Set<Position> fallenBytes = simulateBytes(bytes, timestep);
    final int result1 = djikstra(fallenBytes);
    System.out.println("Part 1: " + result1);
    final Position result2 = firstCutoffByte(bytes);
    assert result2 != null;
    System.out.println("Part 2: " + result2.x() + "," + result2.y());
  }

  private record Position(int x, int y) {

  }
}
