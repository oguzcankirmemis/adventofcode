package org.day23;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.common.Toolbox;

public class Main {

  private static final String inputFile = "input_day23.txt";

  private Main() {
    super();
  }

  private static Graph parseInput(final String input) {
    final Set<String> nodes = new HashSet<>();
    final Map<String, Set<String>> edges = new HashMap<>();
    Arrays.stream(input.split(System.lineSeparator())).forEach(l -> {
      String[] vertices = l.split("-");
      nodes.add(vertices[0]);
      nodes.add(vertices[1]);
      edges.putIfAbsent(vertices[0], new HashSet<>());
      edges.putIfAbsent(vertices[1], new HashSet<>());
      edges.get(vertices[0]).add(vertices[1]);
      edges.get(vertices[1]).add(vertices[0]);
    });
    return new Graph(nodes, edges);
  }

  private static int dfs(
      final Graph graph,
      final String curr,
      final String source,
      final int len,
      final Set<String> visited
  ) {
    if (len == 2 && graph.edges().get(curr).contains(source)) {
      return 1;
    }
    if (len == 2) {
      return 0;
    }
    int result = 0;
    for (final String next : graph.edges().get(curr)) {
      if (!visited.contains(next)) {
        result += dfs(graph, next, source, len + 1, visited);
      }
    }
    return result;
  }

  private static int countThreeCycles(final Graph graph) {
    int cycles = 0;
    final Set<String> visited = new HashSet<>();
    for (final String node : graph.nodes()) {
      if (node.charAt(0) == 't') {
        visited.add(node);
        cycles += dfs(graph, node, node, 0, visited);
      }
    }
    return cycles / 2;
  }

  private static Set<String> maxClique(
      final Graph graph,
      final Set<String> clique,
      final Set<String> include,
      final Set<String> exclude
  ) {
    if (include.isEmpty() && exclude.isEmpty()) {
      return clique;
    }
    Set<String> max = clique;
    String pivot;
    if (!include.isEmpty()) {
      pivot = include.iterator().next();
    } else {
      pivot = exclude.iterator().next();
    }
    final Set<String> pivotNeighbours = graph.edges().get(pivot);
    final Set<String> includeCopy = new HashSet<>(include);
    for (final String next : includeCopy) {
      if (pivotNeighbours.contains(next)) {
        continue;
      }
      final Set<String> nextNeighbours = graph.edges().get(next);
      final Set<String> nextClique = new HashSet<>(clique);
      nextClique.add(next);
      final Set<String> nextInclude = new HashSet<>(include);
      nextInclude.retainAll(nextNeighbours);
      final Set<String> nextExclude = new HashSet<>(exclude);
      nextExclude.retainAll(nextNeighbours);
      final Set<String> nextMaxClique = maxClique(graph, nextClique, nextInclude, nextExclude);
      if (nextMaxClique.size() > max.size()) {
        max = nextMaxClique;
      }
      include.remove(next);
      exclude.add(next);
    }
    return max;
  }

  private static String[] sorted(final Set<String> nodes) {
    final String[] sortedNodes = nodes.toArray(new String[0]);
    Arrays.sort(sortedNodes, String::compareTo);
    return sortedNodes;
  }

  private static String nodesToString(final String[] nodes) {
    final StringBuilder sb = new StringBuilder();
    for (final String node : nodes) {
      if (!sb.isEmpty()) {
        sb.append(",");
      }
      sb.append(node);
    }
    return sb.toString();
  }

  public static void main(final String[] args) {
    final String input = Toolbox.getInput(inputFile);
    final Graph graph = parseInput(input);
    final int result1 = countThreeCycles(graph);
    System.out.println("Part 1: " + result1);
    final String result2 = nodesToString(
        sorted(
            maxClique(
                graph,
                new HashSet<>(),
                new HashSet<>(graph.nodes()),
                new HashSet<>()
            )
        )
    );
    System.out.println("Part 2: " + result2);
  }

  private record Graph(Set<String> nodes, Map<String, Set<String>> edges) {

  }
}
