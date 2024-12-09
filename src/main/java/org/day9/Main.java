package org.day9;

import org.common.Toolbox;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Main {
    private record Block(int id, int length) {}

    private static final int FREE_SPACE = -1;
    private static final String inputFile = "input_day9.txt";

    private Main() {
        super();
    }

    private static Block[] parseInput(final String input) {
        final int[] disk = input.chars().map(d -> Integer.parseInt("" + (char) d)).toArray();
        final Block[] blocks = new Block[disk.length];
        int fileId = 0;
        for (int i = 0; i < disk.length; i++) {
            if (i % 2 == 0) {
                blocks[i] = new Block(fileId, disk[i]);
                fileId++;
            } else {
                blocks[i] = new Block(-1, disk[i]);
            }
        }
        return blocks;
    }

    private static int[] expanded(final Block[] input) {
        final int length = Arrays.stream(input)
                .reduce(0, (acc, curr) -> acc + curr.length(), Integer::sum);
        final int[] expanded = new int[length];
        int index = 0;
        for (final Block block : input) {
            int blockLength = block.length();
            while (0 < blockLength) {
                expanded[index] = block.id();
                index++;
                blockLength--;
            }
        }
        return expanded;
    }

    private static int[] consolidateByFragment(final Block[] disk) {
        final int[] expanded = Main.expanded(disk);
        int start = 0;
        int end = expanded.length - 1;
        while (start <= end) {
            if (Main.FREE_SPACE != expanded[start]) {
                start++;
            } else if (Main.FREE_SPACE == expanded[end]) {
                end--;
            } else {
                expanded[start] = expanded[end];
                expanded[end] = Main.FREE_SPACE;
                end--;
                start++;
            }
        }
        return expanded;
    }

    private static int[] consolidateByFile(final Block[] disk) {
        final Block[] copy = disk.clone();
        final List<Block> consolidated = new ArrayList<Block>();
        for (int i = 0; i < copy.length; i++) {
            Block block = copy[i];
            if (Main.FREE_SPACE != block.id()) {
                consolidated.add(block);
                continue;
            }
            int end = copy.length - 1;
            while (0 < block.length() && end > i) {
                final Block file = copy[end];
                if (Main.FREE_SPACE == file.id() || file.length() > block.length()) {
                    end--;
                    continue;
                }
                consolidated.add(file);
                copy[end] = new Block(Main.FREE_SPACE, file.length());
                block = new Block(block.id(), block.length() - file.length());
                end = copy.length - 1;
            }
            if (0 < block.length()) {
                consolidated.add(block);
            }
        }
        return Main.expanded(consolidated.toArray(new Block[] {}));
    }

    private static long checksum(final int[] disk) {
        long checksum = 0L;
        for (int i = 0; i < disk.length; i++) {
            if (Main.FREE_SPACE != disk[i]) {
                checksum += (long) (i * disk[i]);
            }
        }
        return checksum;
    }

    public static void main(final String[] args) {
        final String input = Toolbox.getInput(Main.inputFile);
        final Block[] disk = Main.parseInput(input);
        final long result1 = Main.checksum(Main.consolidateByFragment(disk));
        System.out.println("Part 1: " + result1);
        final long result2 = Main.checksum(Main.consolidateByFile(disk));
        System.out.println("Part 2: " + result2);
    }
}
