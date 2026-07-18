#!/bin/bash

input=$(cat inputs/input.txt)
length=${#input}
sum=0

for (( i=0; i<${#input}; i++ )); do
  next=$(( (i + 1) % length ))
  if [[ "${input:${i}:1}" == "${input:${next}:1}" ]]; then
    digit=${input:${i}:1}
    sum=$(( sum + digit ))
  fi
done

echo "Part 1: ${sum}"

sum=0
for (( i=0; i<${#input}; i++ )); do
  next=$(( (i + (length / 2))  % length ))
  if [[ "${input:${i}:1}" == "${input:${next}:1}" ]]; then
    digit=${input:${i}:1}
    sum=$(( sum + digit ))
  fi
done

echo "Part 2: ${sum}"
