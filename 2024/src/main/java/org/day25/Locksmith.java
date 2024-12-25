package org.day25;

import java.util.ArrayList;
import java.util.List;

public class Locksmith {

  private final List<Key> keys = new ArrayList<>();
  private final List<Lock> locks = new ArrayList<>();

  public void addKey(final Key key) {
    keys.add(key);
  }

  public void addLock(final Lock lock) {
    locks.add(lock);
  }

  public int combinations() {
    int passing = 0;
    for (final Key key : keys) {
      for (final Lock lock : locks) {
        if (key.heights().length != lock.heights().length) {
          continue;
        }
        boolean fits = true;
        for (int i = 0; i < key.heights().length; i++) {
          if (key.heights()[i] + lock.heights()[i] >= lock.maxHeight()) {
            fits = false;
            break;
          }
        }
        if (fits) {
          passing++;
        }
      }
    }
    return passing;
  }

  @Override
  public String toString() {
    return keys + System.lineSeparator() + locks;
  }
}
