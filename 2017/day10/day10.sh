#!/bin/bash

size=256
list=()

for ((i=0; i<${size}; i++)); do
  list[${i}]=${i}
done

IFS="," read -a input < inputs/input.txt

skip=0
index=0

for length in "${input[@]}"; do
  if (( ${length} >= ${size} )); then
    continue
  fi
  if (( ${length} == 0 )); then
    index=$(( (index  + skip) % size ))
    skip=$(( skip + 1 ))
    continue
  fi
  i=${index}
  j=$(( (i + length - 1) % size ))
  while (( ${i} != ${j} )); do
    t=${list[${j}]}
    list[${j}]=${list[${i}]}
    list[${i}]=${t}
    i=$(( (i + 1) % size ))
    if (( ${i} == ${j} )); then
      break
    fi
    j=$(( j - 1 ))
    if (( ${j} < 0 )); then
      j=$(( size - 1 ))
    fi
  done
  index=$(( ( index + length + skip ) % size ))
  skip=$(( skip + 1 ))
done

result=$(( list[0] * list[1] ))

echo "Part 1: ${result}"

result=""
rounds=64
index=0
skip=0
sequence=()
input=$(cat inputs/input.txt)

for ((i=0; i<${#input}; i++)); do
  sequence[${#sequence[@]}]=$(printf "%d" "'${input:${i}:1}")
done
sequence[${#sequence[@]}]=17
sequence[${#sequence[@]}]=31
sequence[${#sequence[@]}]=73
sequence[${#sequence[@]}]=47
sequence[${#sequence[@]}]=23

for ((i=0; i<${size}; i++)); do
  list[${i}]=${i}
done

while (( ${rounds} > 0 )); do
  for length in "${sequence[@]}"; do
    if (( ${length} >= ${size} )); then
      continue
    fi
    if (( ${length} == 0 )); then
      index=$(( (index  + skip) % size ))
      skip=$(( skip + 1 ))
      continue
    fi
    i=${index}
    j=$(( (i + length - 1) % size ))
    while (( ${i} != ${j} )); do
      t=${list[${j}]}
      list[${j}]=${list[${i}]}
      list[${i}]=${t}
      i=$(( (i + 1) % size ))
      if (( ${i} == ${j} )); then
        break
      fi
      j=$(( j - 1 ))
      if (( ${j} < 0 )); then
        j=$(( size - 1 ))
      fi
    done
    index=$(( ( index + length + skip ) % size ))
    skip=$(( skip + 1 ))
  done
  rounds=$(( rounds - 1 ))
done

for ((i=0; i<${size}/16; i++)); do
  block=0
  for ((j=0; j<16; j++)); do
    block=$(( block ^ list[16 * i + j] ))
  done
  hex=$(printf "%02x" "${block}")
  result="${result}${hex}"
done

echo "Part 2: ${result}"

