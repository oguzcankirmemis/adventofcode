package org.day14;

import java.util.Arrays;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day14.txt";

  private Main() {
    super();
  }

  private static int mod(int a, int b) {
    if (a >= 0) {
      return a % b;
    }
    return (b + (a % b)) % b;
  }

  private static Robot parseRobot(final String input) {
    final String[] posVel = input.split(" ");
    final String pos = posVel[0];
    final String vel = posVel[1];
    final String[] xyPos = pos.substring(2).split(",");
    final String[] xyVel = vel.substring(2).split(",");
    final Position p = new Position(Integer.parseInt(xyPos[0]), Integer.parseInt(xyPos[1]));
    final Velocity v = new Velocity(Integer.parseInt(xyVel[0]), Integer.parseInt(xyVel[1]));
    return new Robot(p, v);
  }

  private static Robot[] parseInput(final String input) {
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(Main::parseRobot)
        .toArray(Robot[]::new);
  }

  private static Position[] simulate(
      final int width,
      final int height,
      final int seconds,
      final Robot[] robots
  ) {
    return Arrays.stream(robots)
        .map(r -> r.simulate(width, height, seconds))
        .toArray(Position[]::new);
  }

  private static int calculateQuadrants(
      final int width,
      final int height,
      final Position[] robots
  ) {
    final int midWidth = width / 2;
    final int midHeight = height / 2;
    final int[] quadrants = new int[]{0, 0, 0, 0};
    Arrays.stream(robots).forEach(p -> {
      if (p.isInFirstQuadrant(midWidth, midHeight)) {
        quadrants[0]++;
      }
      if (p.isInSecondQuadrant(midWidth, midHeight)) {
        quadrants[1]++;
      }
      if (p.isInThirdQuadrant(midWidth, midHeight)) {
        quadrants[2]++;
      }
      if (p.isInFourthQuadrant(midWidth, midHeight)) {
        quadrants[3]++;
      }
    });
    return Arrays.stream(quadrants).reduce(1, (acc, curr) -> acc * curr);
  }

  private static char[][] toRoom(final int width, final int height, final Position[] robots) {
    final char[][] room = new char[height][width];
    for (int i = 0; i < height; i++) {
      for (int j = 0; j < width; j++) {
        room[i][j] = '.';
      }
    }
    Arrays.stream(robots).forEach(r -> room[r.y()][r.x()] = 'X');
    return room;
  }

  private static int bruteforce(
      final int width,
      final int height,
      final int seconds,
      final Robot[] robots
  ) {
    int secondsForMaxConsecutive = -1;
    int maxConsecutive = 0;
    for (int i = 0; i <= seconds; i++) {
      final Position[] positions = simulate(width, height, i, robots);
      final char[][] room = toRoom(width, height, positions);
      for (int j = 0; j < height; j++) {
        int consecutive = 0;
        for (int k = 1; k < width; k++) {
          if (room[j][k] == 'X' && room[j][k - 1] == 'X') {
            consecutive++;
          } else {
            if (consecutive > maxConsecutive) {
              maxConsecutive = consecutive;
              secondsForMaxConsecutive = i;
            }
            consecutive = 0;
          }
        }
      }
    }
    return secondsForMaxConsecutive;
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Robot[] robots = parseInput(input);
    final int width = 101;
    final int height = 103;
    final int seconds = 100;
    final int maxSeconds = width * height;
    final Position[] positions = simulate(width, height, seconds, robots);
    final int result1 = calculateQuadrants(width, height, positions);
    System.out.println("Part 1: " + result1);
    final int result2 = bruteforce(width, height, maxSeconds, robots);
    System.out.println("Part 2: " + result2);
  }

  private record Position(int x, int y) {

    private boolean isInFirstQuadrant(int midWidth, int midHeight) {
      return x < midWidth && y < midHeight;
    }

    private boolean isInSecondQuadrant(int midWidth, int midHeight) {
      return x > midWidth && y < midHeight;
    }

    private boolean isInThirdQuadrant(int midWidth, int midHeight) {
      return x < midWidth && y > midHeight;
    }

    private boolean isInFourthQuadrant(int midWidth, int midHeight) {
      return x > midWidth && y > midHeight;
    }
  }

  private record Velocity(int x, int y) {

  }

  private record Robot(Position p, Velocity v) {

    private Position simulate(int width, int height, int seconds) {
      final int xPos = mod(p.x() + v.x() * seconds, width);
      final int yPos = mod(p.y() + v.y() * seconds, height);
      return new Position(xPos, yPos);
    }

  }
}
