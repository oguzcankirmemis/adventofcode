package org.day18;

import java.util.Arrays;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day18.txt";

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

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Position[] bytes = parseInput(input);
    System.out.println(bytes.length);
  }

  private record Position(int x, int y) {

  }
}
