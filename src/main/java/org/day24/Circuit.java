package org.day24;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;

public class Circuit {

  private final static int INCORRECT_PAIRS = 4;
  private final static int MAX_OUTPUT_BITS = 64;
  private final static String REGISTER_FORMAT_STRING = "%2s";
  private final static String INPUT_REGISTER_1 = "x";
  private final static String INPUT_REGISTER_2 = "y";
  private final static String OUTPUT_REGISTER = "z";

  private final Map<String, List<Gate>> inGateMap = new HashMap<>();
  private final Map<String, Gate> outGateMap = new HashMap<>();
  private final List<Gate> executables = new ArrayList<>();
  private Map<String, Boolean> originalWires = null;
  private Map<String, Boolean> wires = new HashMap<>();

  public void addWire(final String wire, final boolean value) {
    wires.put(wire, value);
  }

  public void addGate(final GateType type, final String in1, final String in2, final String out) {
    final Gate gate = new Gate(type, in1, in2, out);
    inGateMap.putIfAbsent(in1, new ArrayList<>());
    inGateMap.putIfAbsent(in2, new ArrayList<>());
    inGateMap.get(in1).add(gate);
    inGateMap.get(in2).add(gate);
    outGateMap.put(out, gate);
    if (wires.containsKey(in1) && wires.containsKey(in2)) {
      executables.add(gate);
    }
  }

  public long execute() {
    if (originalWires == null) {
      originalWires = new HashMap<>(wires);
    }
    while (!executables.isEmpty()) {
      final Gate gate = executables.removeLast();
      gate.execute(wires);
      executables.addAll(
          inGateMap.getOrDefault(gate.out(), List.of())
              .stream()
              .filter(g -> wires.containsKey(g.in1()) && wires.containsKey(g.in2()))
              .toList()
      );
    }
    return computeRegister(OUTPUT_REGISTER);
  }

  public String getRegisterWire(final String register, int index) {
    return String.format(register + REGISTER_FORMAT_STRING, index).replace(" ", "0");
  }

  public long computeRegister(final String register) {
    long result = 0L;
    for (int i = MAX_OUTPUT_BITS - 1; i >= 0; i--) {
      final String indexStr = getRegisterWire(register, i);
      result = result | (wires.getOrDefault(indexStr, false) ? 1L : 0L);
      if (i != 0) {
        result = result << 1;
      }
    }
    return result;
  }

  public void reset() {
    wires = new HashMap<>(originalWires);
    executables.clear();
    final Set<String> wiresSet = wires.keySet();
    for (final String wire : wiresSet) {
      for (final Gate gate : inGateMap.get(wire)) {
        if (wiresSet.contains(gate.in1()) && wiresSet.contains(gate.in2())) {
          executables.add(gate);
        }
      }
    }
  }

  public List<List<Gate>> getDependencyList(final String register, final int index) {
    final List<List<Gate>> dependencies = new ArrayList<>();
    final String root = getRegisterWire(register, index);
    final Queue<List<Gate>> workingSet = new LinkedList<>();
    workingSet.add(List.of(outGateMap.get(root)));
    while (!workingSet.isEmpty()) {
      final List<Gate> levelGates = workingSet.poll();
      dependencies.add(levelGates);
      final List<Gate> nextLevel = new ArrayList<>();
      for (final Gate gate : levelGates) {
        if (outGateMap.containsKey(gate.in1())) {
          nextLevel.add(outGateMap.get(gate.in1()));
        }
        if (outGateMap.containsKey(gate.in2())) {
          nextLevel.add(outGateMap.get(gate.in2()));
        }
      }
      if (!nextLevel.isEmpty()) {
        workingSet.add(nextLevel);
      }
    }
    return dependencies;
  }

  public Optional<Gate> findGate(final GateType type, final String w1, final String w2) {
    return inGateMap.get(w1).stream()
        .filter(g -> Set.of(w1, w2).contains(g.in1()))
        .filter(g -> Set.of(w1, w2).contains(g.in2()))
        .filter(g -> g.type() == type)
        .findFirst();
  }

  public void swapGateOutputWires(final Gate g1, final Gate g2) {
    final Gate correctedGate1 = new Gate(g1.type(), g1.in1(), g1.in2(), g2.out());
    final Gate correctedGate2 = new Gate(g2.type(), g2.in2(), g2.in2(), g1.out());
    List<Gate> g1in1 = inGateMap.get(g1.in1());
    List<Gate> g1in2 = inGateMap.get(g1.in2());
    List<Gate> g2in1 = inGateMap.get(g2.in1());
    List<Gate> g2in2 = inGateMap.get(g2.in2());
    g1in1.set(g1in1.indexOf(g1), correctedGate1);
    g1in2.set(g1in2.indexOf(g1), correctedGate1);
    g2in1.set(g2in1.indexOf(g2), correctedGate2);
    g2in2.set(g2in2.indexOf(g2), correctedGate2);
    outGateMap.put(g1.out(), g2);
    outGateMap.put(g2.out(), g1);
  }

  public List<String> fixCircuit() {
    List<String> incorrectWires = new ArrayList<>();
    int fixedPairs = 0;
    int index = 2;
    while (fixedPairs != INCORRECT_PAIRS) {
      final String o = getRegisterWire(OUTPUT_REGISTER, index);
      final String r1 = getRegisterWire(INPUT_REGISTER_1, index);
      final String r2 = getRegisterWire(INPUT_REGISTER_2, index);
      final String prevO = getRegisterWire(OUTPUT_REGISTER, index - 1);
      final String prevR1 = getRegisterWire(INPUT_REGISTER_1, index - 1);
      final String prevR2 = getRegisterWire(INPUT_REGISTER_2, index - 1);
      final Gate root = outGateMap.get(o);
      final Gate prevRoot = outGateMap.get(prevO);
      final Gate currentXor = findGate(GateType.XOR, r1, r2).orElseThrow();
      final Gate prevCarry = findGate(GateType.AND, prevRoot.in1(), prevRoot.in2()).orElseThrow();
      final Gate prevBitsAnd = findGate(GateType.AND, prevR1, prevR2).orElseThrow();
      final Optional<Gate> orOptional = findGate(GateType.OR, prevCarry.out(), prevBitsAnd.out());
      Gate or;
      if (orOptional.isEmpty()) {
        or = outGateMap.get(root.in1());
        if (or.type() != GateType.OR) {
          or = outGateMap.get(root.in2());
        }
        if (or.type() != GateType.XOR) {
          throw new RuntimeException("Ambiguous error in one output bit.");
        }
        final Set<String> orInputs = Set.of(or.in1(), or.in2());
        if (orInputs.contains(prevCarry.out()) && orInputs.contains(prevBitsAnd.out())) {
          throw new RuntimeException("Ambiguous error in one output bit.");
        }
        Gate wrongGate = prevCarry;
        Gate correctGate = prevBitsAnd;
        if (orInputs.contains(prevCarry.out())) {
          wrongGate = prevBitsAnd;
          correctGate = prevCarry;
        }
        Gate replacement = outGateMap.get(or.in1());
        if (correctGate.out().equals(or.in1())) {
          replacement = outGateMap.get(or.in2());
        }
        swapGateOutputWires(wrongGate, replacement);
        incorrectWires.addAll(List.of(wrongGate.out(), replacement.out()));
        fixedPairs++;
      } else {
        or = orOptional.get();
      }
      final Optional<Gate> rootXorOptional = findGate(GateType.XOR, or.out(), currentXor.out());
      Gate rootXor;
      if (rootXorOptional.isEmpty()) {
        rootXor = root;
        if (root.type() != GateType.XOR) {
          throw new RuntimeException("Ambiguous error in one output bit.");
        }
        final Set<String> rootInputs = Set.of(root.in1(), root.in2());
        if (!rootInputs.contains(or.out()) && !rootInputs.contains(currentXor.out())) {
          throw new RuntimeException("Ambiguous error in one output bit.");
        }
        Gate wrongGate = or;
        Gate correctGate = currentXor;
        if (rootInputs.contains(or.out())) {
          wrongGate = currentXor;
          correctGate = or;
        }
        Gate replacement = outGateMap.get(root.in1());
        if (correctGate.out().equals(root.in1())) {
          replacement = outGateMap.get(root.in2());
        }
        swapGateOutputWires(wrongGate, replacement);
        incorrectWires.addAll(List.of(wrongGate.out(), replacement.out()));
        fixedPairs++;
      } else {
        rootXor = rootXorOptional.get();
      }
      if (!rootXor.out().equals(root.out())) {
        swapGateOutputWires(rootXor, root);
        incorrectWires.addAll(List.of(rootXor.out(), root.out()));
        fixedPairs++;
      }
      index++;
    }
    return incorrectWires;
  }
}
