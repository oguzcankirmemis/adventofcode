package org.day25;

import java.util.Arrays;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day25.txt";

  private Main() {
    super();
  }

  private static Locksmith parseInput(final String input) {
    final Locksmith locksmith = new Locksmith();
    Arrays.stream(input.split(System.lineSeparator() + System.lineSeparator()))
        .forEach(s -> {
          if (s.charAt(0) == '#') {
            locksmith.addLock(Lock.parseLock(s));
          }
          if (s.charAt(0) == '.') {
            locksmith.addKey(Key.parseKey(s));
          }
        });
    return locksmith;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Locksmith locksmith = parseInput(input);
    final int result1 = locksmith.combinations();
    System.out.println("Part 1: " + result1);
  }
}
