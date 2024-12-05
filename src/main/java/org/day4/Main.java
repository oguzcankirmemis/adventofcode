package org.day4;

import org.common.Toolbox;

import java.util.Arrays;
import java.util.List;
import java.util.Stack;

public class Main {
    private static final String inputFile = "input_day4.txt";

    private Main() {
        super();
    }

    private static char[][] parseInput(final String input) {
        return Arrays.stream(input.split(System.lineSeparator())).map(String::toCharArray).toArray(char[][]::new);
    }

    private static boolean readString(
            final char[][] map,
            final String str,
            int x,
            int y,
            final int xDir,
            final int yDir
    ) {
        final String reverse = new StringBuilder(str).reverse().toString();
        final var stack = new Stack<Character>();
        stack.addAll(List.of(reverse.chars().mapToObj(c -> (char) c).toArray(Character[]::new)));
        while (!stack.isEmpty() && 0 <= y && y < map.length && 0 <= x && x < map[y].length) {
            final char next = stack.pop();
            if (map[y][x] != next) {
                return false;
            }
            x += xDir;
            y += yDir;
        }
        return stack.isEmpty();
    }

    private static int countXmas(final char[][] map) {
        int count = 0;
        final int[][] dirs = {
                {1, 0},
                {-1, 0},
                {0, 1},
                {0, -1},
                {1, 1},
                {1, -1},
                {-1, 1},
                {-1, -1}
        };
        for (final int[] dir : dirs) {
            for (int y = 0; y < map.length; y++) {
                for (int x = 0; x < map[y].length; x++) {
                    if (Main.readString(map, "XMAS", x, y, dir[0], dir[1])) {
                        count++;
                    }
                }
            }
        }
        return count;
    }

    private static int countCrossMas(final char[][] map) {
        int count = 0;
        final int[][] countMap = new int[map.length][map[0].length];
        final int[][] dirs = {
                {1, 1},
                {1, -1},
                {-1, 1},
                {-1, -1}
        };
        for (final int[] dir : dirs) {
            for (int y = 0; y < map.length; y++) {
                for (int x = 0; x < map[y].length; x++) {
                    if (Main.readString(map, "MAS", x, y, dir[0], dir[1])) {
                        countMap[y + dir[1]][x + dir[0]]++;
                        if (2 == countMap[y + dir[1]][x + dir[0]]) {
                            count++;
                        }
                    }
                }
            }
        }
        return count;
    }

    public static void main(final String[] args) {
        final String input = Toolbox.getInput(Main.inputFile);
        final char[][] map = Main.parseInput(input);
        final int result1 = Main.countXmas(map);
        System.out.println("Part 1: " + result1);
        final int result2 = Main.countCrossMas(map);
        System.out.println("Part 2: " + result2);
    }
}
