package org.day25;

import java.util.Arrays;

public record Key(int[] heights) {

  public static Key parseKey(final String key) {
    final char[][] keyMap = Arrays.stream(key.split(System.lineSeparator()))
        .map(String::toCharArray)
        .toArray(char[][]::new);
    final int[] heights = new int[keyMap[0].length];
    for (int i = 0; i < keyMap[0].length; i++) {
      int height = 0;
      for (int j = keyMap.length - 2; j >= 0; j--) {
        if (keyMap[j][i] == '#') {
          height++;
        } else {
          break;
        }
      }
      heights[i] = height;
    }
    return new Key(heights);
  }
}
