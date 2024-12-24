package org.day25;

import java.util.Arrays;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day25.txt";

  private Main() {
    super();
  }

  private static String[] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator())).toArray(String[]::new);
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    System.out.println(input);
  }
}
