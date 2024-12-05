package org.day5;

import org.common.Toolbox;

import java.util.*;

public class Main {
    private static final String inputFile = "input_day5.txt";

    private Main() {
        super();
    }

    private static Map<Integer, Set<Integer>> parseRules(String input) {
        input = input.split(System.lineSeparator() + System.lineSeparator())[0];
        final Map<Integer, Set<Integer>> rules = new HashMap<Integer, Set<Integer>>();
        for (final String rule : input.split(System.lineSeparator())) {
            String[] parts = rule.split("\\|");
            Integer preceder = Integer.valueOf(parts[0]);
            Integer follower = Integer.valueOf(parts[1]);
            rules.putIfAbsent(preceder, new HashSet<Integer>());
            rules.get(preceder).add(follower);
        }
        return rules;
    }

    private static Integer[][] parseUpdates(String input) {
        input = input.split(System.lineSeparator() + System.lineSeparator())[1];
        return Arrays.stream(input.split(System.lineSeparator()))
                .map(update ->
                        Arrays.stream(update.split(","))
                                .map(Integer::valueOf).toArray(Integer[]::new)
                ).toArray(Integer[][]::new);
    }

    private static int getUpdateValue(Map<Integer, Set<Integer>> rules, Integer[] update) {
        final Set<Integer> empty = new HashSet<Integer>();
        for (int i = 0; i < update.length; i++) {
            for (int j = i - 1; 0 <= j; j--) {
                if (rules.getOrDefault(update[i], empty).contains(update[j])) {
                    return 0;
                }
            }
        }
        return update[update.length / 2];
    }

    private static int sumValidMiddlePages(final Map<Integer, Set<Integer>> rules, final Integer[][] updates) {
        return Arrays.stream(updates).reduce(
                0,
                (total, update) -> total + Main.getUpdateValue(rules, update),
                Integer::sum
        );
    }

    public static void main(final String[] args) {
        final String input = Toolbox.getInput(inputFile);
        final Map<Integer, Set<Integer>> rules = Main.parseRules(input);
        final Integer[][] updates = Main.parseUpdates(input);
        final int result1 = Main.sumValidMiddlePages(rules, updates);
        System.out.println(result1);
    }
}
