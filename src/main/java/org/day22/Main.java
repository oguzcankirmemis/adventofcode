package org.day22;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day22.txt";

  private Main() {
    super();
  }

  private static PseudoRandomGenerator[] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(Long::parseLong)
        .map(PseudoRandomGenerator::new)
        .toArray(PseudoRandomGenerator[]::new);
  }

  private static long bruteforce(
      final PseudoRandomGenerator[] sequences,
      final int steps
  ) {
    final Map<ChangeSequence, Long> sequenceMap = new HashMap<>();
    long max = 0L;
    for (final PseudoRandomGenerator s : sequences) {
      final Set<ChangeSequence> localSequenceSet = new HashSet<>();
      for (int i = 0; i < steps; i++) {
        s.next();
        final ChangeSequence change = s.getChangeSequence();
        if (change == null || localSequenceSet.contains(change)) {
          continue;
        }
        final long curr = sequenceMap.getOrDefault(change, 0L);
        sequenceMap.put(change, curr + s.peekPrice());
        localSequenceSet.add(change);
      }
    }
    for (final long total : sequenceMap.values()) {
      max = Math.max(max, total);
    }
    return max;
  }

  private static long sum(final PseudoRandomGenerator[] sequences) {
    return Arrays.stream(sequences).reduce(0L, (acc, curr) -> acc + curr.peek(), Long::sum);
  }

  private static long next(final PseudoRandomGenerator[] sequences, final int steps) {
    for (int i = 0; i < steps; i++) {
      Arrays.stream(sequences).forEach(PseudoRandomGenerator::next);
    }
    long result = sum(sequences);
    Arrays.stream(sequences).forEach(PseudoRandomGenerator::reset);
    return result;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final PseudoRandomGenerator[] sequences = parseInput(input);
    final long result1 = next(sequences, 2000);
    System.out.println("Part 1: " + result1);
    final long result2 = bruteforce(sequences, 2000);
    System.out.println("Part 2: " + result2);
  }
}
