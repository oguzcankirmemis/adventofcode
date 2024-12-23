package org.day24;

import java.util.Arrays;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day24.txt";

  private Main() {
    super();
  }

  private static String[] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .toArray(String[]::new);
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final String[] parsed = parseInput(input);
    System.out.println(parsed.length);
  }
}
