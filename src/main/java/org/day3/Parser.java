package org.day3;

import java.util.Optional;

public class Parser {
    private static final String MUL = "mul";
    private static final String LEFT_PARA = "(";
    private static final String RIGHT_PARA = ")";
    private static final String COMMA = ",";
    private static final String DO = "do()";
    private static final String DONT = "don't()";

    private final String input;
    private int pos;

    Parser(final String input) {
        super();
        this.input = input;
        this.pos = 0;
    }

    private boolean isMul() {
        final boolean hit = this.input.startsWith(Parser.MUL, this.pos);
        if (hit) {
            this.pos += Parser.MUL.length();
        }
        return hit;
    }

    private boolean isLeftParanthesis() {
        final boolean hit = this.input.startsWith(Parser.LEFT_PARA, this.pos);
        if (hit) {
            this.pos += Parser.LEFT_PARA.length();
        }
        return hit;
    }

    private boolean isRightParanthesis() {
        final boolean hit = this.input.startsWith(Parser.RIGHT_PARA, this.pos);
        if (hit) {
            this.pos += Parser.RIGHT_PARA.length();
        }
        return hit;
    }

    private boolean isComma() {
        final boolean hit = this.input.startsWith(Parser.COMMA, this.pos);
        if (hit) {
            this.pos += Parser.COMMA.length();
        }
        return hit;
    }

    private boolean isDo() {
        final boolean hit = this.input.startsWith(Parser.DO, this.pos);
        if (hit) {
            this.pos += Parser.DO.length();
        }
        return hit;
    }

    private boolean isDont() {
        final boolean hit = this.input.startsWith(Parser.DONT, this.pos);
        if (hit) {
            this.pos += Parser.DONT.length();
        }
        return hit;
    }

    private Optional<Integer> parseNumber() {
        final StringBuilder num = new StringBuilder(3);
        while (this.pos < this.input.length() && 3 > num.length() && Character.isDigit(this.input.charAt(this.pos))) {
            num.append(this.input.charAt(this.pos));
            this.pos++;
        }
        if (num.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(Integer.valueOf(num.toString()));
    }

    private Optional<Integer> parseExpression() {
        if (!this.isMul()) {
            return Optional.empty();
        }
        if (!this.isLeftParanthesis()) {
            return Optional.empty();
        }
        final Optional<Integer> num1 = this.parseNumber();
        if (num1.isEmpty()) {
            return Optional.empty();
        }
        if (!this.isComma()) {
            return Optional.empty();
        }
        final Optional<Integer> num2 = this.parseNumber();
        if (num2.isEmpty()) {
            return Optional.empty();
        }
        if (!this.isRightParanthesis()) {
            return Optional.empty();
        }
        return Optional.of(num1.get() * num2.get());
    }

    int collectResultPart1() {
        int result = 0;
        while (this.pos < this.input.length()) {
            final int memo = this.pos;
            final Optional<Integer> expression = this.parseExpression();
            result += expression.orElse(0);
            if (memo == this.pos) {
                this.pos++;
            }
        }
        return result;
    }

    int collectResultPart2() {
        int result = 0;
        boolean enabled = true;
        while (this.pos < this.input.length()) {
            final int memo = this.pos;
            if (this.isDo()) {
                enabled = true;
            } else if (this.isDont()) {
                enabled = false;
            } else if (enabled) {
                final Optional<Integer> expression = this.parseExpression();
                result += expression.orElse(0);
            }
            if (memo == this.pos) {
                this.pos++;
            }
        }
        return result;
    }

    void reset() {
        this.pos = 0;
    }
}
