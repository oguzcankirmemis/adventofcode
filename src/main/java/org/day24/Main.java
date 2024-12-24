package org.day24;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day24.txt";

  private Main() {
    super();
  }

  private static void parseWires(final String input, final Circuit circuit) {
    Arrays.stream(input.split(System.lineSeparator())).forEach(l -> {
      final String[] parts = l.split(": ");
      final String wire = parts[0];
      final boolean value = parts[1].equals("1");
      circuit.addWire(wire, value);
    });
  }

  private static void parseGates(final String input, final Circuit circuit) {
    Arrays.stream(input.split(System.lineSeparator())).forEach(l -> {
      final String[] parts = l.split(" -> ");
      final String operation = parts[0];
      final String out = parts[1];
      final String[] operationParts = operation.split(" ");
      final String in1 = operationParts[0];
      final GateType type = GateType.valueOf(operationParts[1]);
      final String in2 = operationParts[2];
      circuit.addGate(type, in1, in2, out);
    });
  }

  private static Circuit parseInput(final String input) {
    final Circuit circuit = new Circuit();
    final String[] parts = input.split(System.lineSeparator() + System.lineSeparator());
    parseWires(parts[0], circuit);
    parseGates(parts[1], circuit);
    return circuit;
  }

  private static List<Integer> findIncorrectBits(long actual, long expected) {
    final List<Integer> incorrectBits = new ArrayList<>();
    int i = 0;
    while (actual != 0 || expected != 0) {
      if ((actual & 1L) != (expected & 1L)) {
        incorrectBits.add(i);
      }
      actual >>= 1;
      expected >>= 1;
      i++;
    }
    return incorrectBits;
  }

  private static String formatIncorrectOutputWires(final List<String> wires) {
    final String[] sorted = wires.toArray(new String[0]);
    Arrays.sort(sorted, String::compareTo);
    return Arrays.stream(sorted)
        .reduce("", (curr, acc) -> curr + "," + acc)
        .substring(1);
  }
  
  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Circuit circuit = parseInput(input);
    final long reg1 = circuit.computeRegister("x");
    final long reg2 = circuit.computeRegister("y");
    final long expected = reg1 + reg2;
    final long result1 = circuit.execute();
    System.out.println("Part 1: " + result1);
    circuit.reset();
    final List<String> wrongWires = circuit.fixCircuit();
    final long actual = circuit.execute();
    assert expected == actual;
    final String result2 = formatIncorrectOutputWires(wrongWires);
    System.out.println("Part 2: " + result2);
  }
}
