package org.day22;

import java.util.LinkedList;
import java.util.Queue;

public class PseudoRandomGenerator {

  private static final long PRUNE = 16777216L;
  private static final int PROCESS_1 = 6;
  private static final int PROCESS_2 = 5;
  private static final int PROCESS_3 = 11;

  private final Queue<Long> changes = new LinkedList<>();
  private final long seed;
  private long current;


  public PseudoRandomGenerator(final long initial) {
    this.seed = initial;
    this.current = initial;
  }

  public void next() {
    final long currentPrice = current % 10L;
    long tmp = current << PROCESS_1;
    current = tmp ^ current;
    current = current % PRUNE;
    tmp = current >> PROCESS_2;
    current = tmp ^ current;
    current = current % PRUNE;
    tmp = current << PROCESS_3;
    current = tmp ^ current;
    current = current % PRUNE;
    final long nextPrice = current % 10L;
    changes.add(nextPrice - currentPrice);
    if (changes.size() > 4) {
      changes.remove();
    }
  }

  public long peek() {
    return current;
  }

  public long peekPrice() {
    return current % 10L;
  }

  public ChangeSequence getChangeSequence() {
    if (changes.size() != 4) {
      return null;
    }
    int index = 0;
    final long[] seq = new long[4];
    for (final long e : changes) {
      seq[index] = e;
      index++;
    }
    return new ChangeSequence(seq[0], seq[1], seq[2], seq[3]);
  }

  public void reset() {
    changes.clear();
    current = seed;
  }
}
