package org.day2;

import org.common.Toolbox;

import java.util.Arrays;
import java.util.regex.Pattern;

public class Main {
    private static final Pattern inputPattern = Pattern.compile("\\s+");
    private static final String inputFile = "input_day2.txt";

    private Main() {
        super();
    }

    private static int[][] parseInput(final String input) {
        return Arrays.stream(input.split(System.lineSeparator())).map(line ->
                Arrays.stream(Main.inputPattern.split(line)).map(Integer::valueOf).mapToInt(v -> v).toArray()
        ).toArray(int[][]::new);
    }

    private static boolean isSafe(final int[] report) {
        if (1 > report.length) {
            return true;
        }
        final boolean increasing = 0 < (report[1] - report[0]);
        for (int j = 1; j < report.length; j++) {
            final int diff = report[j] - report[j - 1];
            final int absDiff = Math.abs(diff);
            if (1 > absDiff || 3 < absDiff) {
                return false;
            }
            if (0 > diff && increasing) {
                return false;
            }
            if (0 < diff && !increasing) {
                return false;
            }
        }
        return true;
    }

    private static int[] skipIndex(final int[] report, final int index) {
        final int[] newReport = new int[report.length - 1];
        System.arraycopy(report, 0, newReport, 0, index);
        System.arraycopy(report, index + 1, newReport, index, newReport.length - index);
        return newReport;
    }

    private static int solve1(final int[][] input) {
        int numOfSafe = 0;
        for (final int[] report : input) {
            if (Main.isSafe(report)) {
                numOfSafe++;
            }
        }
        return numOfSafe;
    }

    private static int solve2(final int[][] input) {
        int numOfSafe = 0;
        for (final int[] report : input) {
            if (Main.isSafe(report)) {
                numOfSafe++;
                continue;
            }
            for (int i = 0; i < report.length; i++) {
                final int[] newReport = Main.skipIndex(report, i);
                if (Main.isSafe(newReport)) {
                    numOfSafe++;
                    break;
                }
            }
        }
        return numOfSafe;
    }

    public static void main(final String[] args) {
        final String inputString = Toolbox.getInput(Main.inputFile);
        final int[][] input = Main.parseInput(inputString);
        final int result1 = Main.solve1(input);
        System.out.println("Part 1: " + result1);
        final int result2 = Main.solve2(input);
        System.out.println("Part 2: " + result2);
    }
}
