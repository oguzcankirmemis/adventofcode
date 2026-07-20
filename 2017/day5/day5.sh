#!/bin/bash

read -a input <<< "$(tr '\n' ' ' < inputs/input.txt)"

index=0
steps=0

while (( ${index} < ${#input[@]} )); do
  new_index=$(( index + input[index] ))
  input[${index}]=$(( input[index] + 1 ))
  index=${new_index}
  steps=$(( steps + 1 ))
done

echo "Part 1: ${steps}"

read -a input <<< "$(tr '\n' ' ' < inputs/input.txt)"

index=0
steps=0

while (( ${index} < ${#input[@]} )); do
  new_index=$(( index + input[index] ))
  if (( input[${index}] < 3 )); then
    input[${index}]=$(( input[index] + 1 ))
  else
    input[${index}]=$(( input[index] - 1 ))
  fi
  index=${new_index}
  steps=$(( steps + 1 ))
done

echo "Part 2: ${steps}"
