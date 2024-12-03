package org.day3;

import org.common.Toolbox;

public class Main {
    private static final String inputFile = "input_day3.txt";

    private Main() {
        super();
    }

    public static void main(final String[] args) {
        final String input = Toolbox.getInput(Main.inputFile);
        final var parser = new Parser(input);
        final int result1 = parser.collectResultPart1();
        System.out.println("Part 1: " + result1);
        parser.reset();
        final int result2 = parser.collectResultPart2();
        System.out.println("Part 2: " + result2);
    }
}
