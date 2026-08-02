#!/bin/bash

dir_y=1
dir_x=0
collected=""
map=()
steps=1

while IFS="" read line; do
  map[${#map[@]}]="${line}"
done < inputs/input.txt

y=0
x=$(expr index "${map[0]}" "|")
x=$(( x - 1 ))

while true; do
  new_x=$(( x + dir_x ))
  new_y=$(( y + dir_y ))
  char="${map[${new_y}]:new_x:1}"
  if [[ "${char}" = " " ]]; then
    if (( ${dir_x} != 0 )); then
      dir_y=${dir_x}
      dir_x=0
    else
      dir_x=${dir_y}
      dir_y=0
    fi
    new_x=$(( x + dir_x ))
    new_y=$(( y + dir_y ))
    char="${map[${new_y}]:new_x:1}"
    if [[ "${char}" = " " ]]; then
      dir_x=$(( -1 * dir_x ))
      dir_y=$(( -1 * dir_y ))
      new_x=$(( x + dir_x ))
      new_y=$(( y + dir_y ))
      char="${map[${new_y}]:new_x:1}"
      if [[ "${char}" = " " ]]; then
        break
      fi
    fi
  fi
  code=$(printf "%d" "'${char}")
  if (( 65 <= ${code} )) && (( ${code} <= 90 )); then
    collected="${collected}${char}"
  fi
  x=${new_x}
  y=${new_y}
  steps=$(( steps + 1 ))
done

echo "Part 1: ${collected}"
echo "Part 2: ${steps}"
