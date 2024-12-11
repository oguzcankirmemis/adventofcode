package org.day11;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day11.txt";

  private Main() {
    super();
  }

  private static List<Long> parseInput(final String input) {
    return Arrays.stream(input.split(" "))
        .map(Long::valueOf)
        .toList();
  }

  private static Stream<Long> blink1(final Stream<Long> stones) {
    return stones.flatMap(stone -> {
      if (stone == 0L) {
        return List.of(1L)
            .stream();
      }
      final String stoneStr = stone.toString();
      if (stoneStr.length() % 2 == 0) {
        final long stone1 = Long.parseLong(stoneStr.substring(0, stoneStr.length() / 2));
        final long stone2 = Long.parseLong(stoneStr.substring(stoneStr.length() / 2));
        return List.of(stone1, stone2)
            .stream();
      }
      return List.of(stone * 2024L)
          .stream();
    });
  }

  private static Map<Long, Long> blink2(final Map<Long, Long> stones) {
    final Map<Long, Long> map = new HashMap<Long, Long>();
    for (final Long key : stones.keySet()) {
      final String keyStr = key.toString();
      if (key == 0L) {
        map.put(1L, stones.get(key) + map.getOrDefault(1L, 0L));
      } else if (keyStr.length() % 2 == 0) {
        final long key1 = Long.parseLong(keyStr.substring(0, keyStr.length() / 2));
        final long key2 = Long.parseLong(keyStr.substring(keyStr.length() / 2));
        map.put(key1, stones.get(key) + map.getOrDefault(key1, 0L));
        map.put(key2, stones.get(key) + map.getOrDefault(key2, 0L));
      } else {
        final long key1 = key * 2024L;
        map.put(key1, stones.get(key) + map.getOrDefault(key1, 0L));
      }
    }
    return map;
  }

  private static Stream<Long> iterateStones1(final List<Long> initial, final int blinks) {
    Stream<Long> current = initial.stream();
    for (int i = 0; i < blinks; i++) {
      current = blink1(current);
    }
    return current;
  }

  private static Map<Long, Long> iterateStones2(final List<Long> initial, final int blinks) {
    Map<Long, Long> current = new HashMap<Long, Long>();
    for (Long val : initial) {
      current.put(val, current.getOrDefault(val, 0L) + 1L);
    }
    for (int i = 0; i < blinks; i++) {
      current = blink2(current);
    }
    return current;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final List<Long> stones = parseInput(input);
    final long result1 = iterateStones1(stones, 25).count();
    System.out.println("Part 1: " + result1);
    final Map<Long, Long> map = iterateStones2(stones, 75);
    final long result2 = map.keySet()
        .stream()
        .reduce(0L, (acc, curr) -> acc + map.get(curr), Long::sum);
    System.out.println("Part 2: " + result2);
  }
}
