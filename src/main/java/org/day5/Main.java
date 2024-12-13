package org.day5;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.common.Toolbox;

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
      rules.get(preceder)
          .add(follower);
    }
    return rules;
  }

  private static Integer[][] parseUpdates(String input) {
    input = input.split(System.lineSeparator() + System.lineSeparator())[1];
    return Arrays.stream(input.split(System.lineSeparator()))
        .map(update ->
            Arrays.stream(update.split(","))
                .map(Integer::valueOf)
                .toArray(Integer[]::new)
        )
        .toArray(Integer[][]::new);
  }

  private static boolean isValidUpdate(Map<Integer, Set<Integer>> rules, Integer[] update) {
    final Set<Integer> empty = new HashSet<Integer>();
    for (int i = 0; i < update.length; i++) {
      for (int j = i - 1; 0 <= j; j--) {
        if (rules.getOrDefault(update[i], empty)
            .contains(update[j])) {
          return false;
        }
      }
    }
    return true;
  }

  private static int getUpdateValue(Map<Integer, Set<Integer>> rules, Integer[] update) {
    if (Main.isValidUpdate(rules, update)) {
      return update[update.length / 2];
    }
    return 0;
  }

  private static int sumValidMiddlePages(final Map<Integer, Set<Integer>> rules,
      final Integer[][] updates) {
    return Arrays.stream(updates)
        .reduce(
            0,
            (total, update) -> total + Main.getUpdateValue(rules, update),
            Integer::sum
        );
  }

  private static void fixFirstInconsistency(final Map<Integer, Set<Integer>> rules,
      final Integer[] update) {
    final Set<Integer> empty = new HashSet<Integer>();
    for (int i = 0; i < update.length; i++) {
      for (int j = i - 1; 0 <= j; j--) {
        if (rules.getOrDefault(update[i], empty)
            .contains(update[j])) {
          Integer tmp = update[j];
          update[j] = update[i];
          update[i] = tmp;
        }
      }
    }
  }

  private static Integer[] fixUpdate(final Map<Integer, Set<Integer>> rules,
      final Integer[] update) {
    while (!isValidUpdate(rules, update)) {
      Main.fixFirstInconsistency(rules, update);
    }
    return update;
  }

  private static int sumInvalidMiddlePages(final Map<Integer, Set<Integer>> rules,
      final Integer[][] updates) {
    return Arrays.stream(updates)
        .filter(update -> !Main.isValidUpdate(rules, update))
        .map(update -> Main.fixUpdate(rules, update))
        .reduce(0, (total, update) -> total + update[update.length / 2], Integer::sum);
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Map<Integer, Set<Integer>> rules = Main.parseRules(input);
    final Integer[][] updates = Main.parseUpdates(input);
    final int result1 = Main.sumValidMiddlePages(rules, updates);
    System.out.println(result1);
    final int result2 = Main.sumInvalidMiddlePages(rules, updates);
    System.out.println(result2);
  }
}
