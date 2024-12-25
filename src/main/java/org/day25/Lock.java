package org.day25;

import java.util.Arrays;

public record Lock(int maxHeight, int[] heights) {

  public static Lock parseLock(final String lock) {
    final char[][] lockMap = Arrays.stream(lock.split(System.lineSeparator()))
        .map(String::toCharArray)
        .toArray(char[][]::new);
    final int maxHeight = lockMap.length - 1;
    final int[] heights = new int[lockMap[0].length];
    for (int i = 0; i < lockMap[0].length; i++) {
      int height = 0;
      for (int j = 1; j < lockMap.length; j++) {
        if (lockMap[j][i] == '#') {
          height++;
        } else {
          break;
        }
      }
      heights[i] = height;
    }
    return new Lock(maxHeight, heights);
  }
}
