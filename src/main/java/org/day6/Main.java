package org.day6;

import org.common.Toolbox;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class Main {
    private record Direction(int x, int y) {}
    private record Position(int x, int y) {}
    private record DirPosition(int x, int y, int dir) {}
    private record SimulationResult(int visited, boolean inBounds) {}

    private static final char OBSTACLE = '#';
    private static final char FREE = '.';
    private static final char GUARD = '^';

    private static final String inputFile = "input_day6.txt";

    private Main() {
        super();
    }

    private static char[][] parseInput(final String input) {
        return Arrays.stream(input.split(System.lineSeparator())).map(String::toCharArray).toArray(char[][]::new);
    }

    private static DirPosition getGuardPosition(final char[][] map) {
        for (int i = 0; i < map.length; i++) {
            for (int j = 0; j < map[i].length; j++) {
                if (Main.GUARD == map[i][j]) {
                    return new DirPosition(j, i, 0);
                }
            }
        }
        throw new IllegalArgumentException("Guard not found!");
    }

    private static boolean isInbounds(final char[][] map, final DirPosition pos) {
        return 0 <= pos.y() && map.length > pos.y() && 0 <= pos.x() && map[0].length > pos.x();
    }

    private static SimulationResult simulateGuard(final char[][] map, DirPosition guard) {
        final Set<DirPosition> visited = new HashSet<DirPosition>(map.length * map[0].length);
        final Set<Position> marked = new HashSet<Position>(map.length * map[0].length);
        final Direction[] dirs = new Direction[] {
                new Direction(0, -1),
                new Direction(1, 0),
                new Direction(0, 1),
                new Direction(-1, 0)
        };
        while (Main.isInbounds(map, guard) && !visited.contains(guard)) {
            visited.add(guard);
            marked.add(new Position(guard.x(), guard.y()));
            DirPosition next = new DirPosition(
                    guard.x() + dirs[guard.dir()].x(),
                    guard.y() + dirs[guard.dir()].y(),
                    guard.dir()
            );
            while (Main.isInbounds(map, next) && Main.OBSTACLE == map[next.y()][next.x()]) {
                final int dir = (next.dir() + 1) % dirs.length;
                next = new DirPosition(guard.x() + dirs[dir].x(), guard.y() + dirs[dir].y(), dir);
            }
            guard = next;
        }
        return new SimulationResult(marked.size(), Main.isInbounds(map, guard));
    }

    private static int countPossibleLoops(final char[][] map, final DirPosition guard) {
        int loops = 0;
        for (int i = 0; i < map.length; i++) {
            for (int j = 0; j < map[i].length; j++) {
                if (Main.FREE == map[i][j]) {
                    map[i][j] = Main.OBSTACLE;
                    final SimulationResult result = Main.simulateGuard(map, guard);
                    map[i][j] = Main.FREE;
                    if (result.inBounds()) {
                        loops++;
                    }
                }
            }
        }
        return loops;
    }

    public static void main(final String[] args) {
        final String input = Toolbox.getInput(Main.inputFile);
        final char[][] map = Main.parseInput(input);
        final DirPosition guard = Main.getGuardPosition(map);
        final SimulationResult result1 = Main.simulateGuard(map, guard);
        System.out.println("Part 1: " + result1.visited());
        final int result2 = Main.countPossibleLoops(map, guard);
        System.out.println("Part 2: " + result2);
    }
}
