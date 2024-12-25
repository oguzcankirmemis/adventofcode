package org.common;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

public class Toolbox {

  public static String getInput(String inputFile) {
    ClassLoader classLoader = ClassLoader.getSystemClassLoader();
    try (InputStream stream = classLoader.getResourceAsStream(inputFile)) {
      if (stream == null) {
        throw new RuntimeException("Unknown error!");
      }
      try (var streamReader = new InputStreamReader(stream); var reader = new BufferedReader(streamReader)) {
        return reader.lines()
            .collect(Collectors.joining(System.lineSeparator()));
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }
}
