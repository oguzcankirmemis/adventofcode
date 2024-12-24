package org.day24;

import java.util.Map;

public record Gate(GateType type, String in1, String in2, String out) {

  public void execute(final Map<String, Boolean> wires) {
    final boolean op1 = wires.get(in1);
    final boolean op2 = wires.get(in2);
    switch (type) {
      case GateType.AND -> wires.put(out, op1 && op2);
      case GateType.OR -> wires.put(out, op1 || op2);
      case GateType.XOR -> wires.put(out, op1 ^ op2);
    }
  }
}
