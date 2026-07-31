#!/bin/bash

a_factor=16807
b_factor=48271
remainder=2147483647
mod=65536
a_mod=4
b_mod=8

a_prev=$(cat inputs/input.txt | grep "Generator A" | cut -d " " -f 5)
b_prev=$(cat inputs/input.txt | grep "Generator B" | cut -d " " -f 5)
result=0

function iterate1 () {
  a_prev=$(( (a_prev * a_factor) % remainder ))
  b_prev=$(( (b_prev * b_factor) % remainder ))
  local compare=$(( (a_prev % mod) - (b_prev % mod) ))
  if (( ${compare} == 0 )); then
    result=$(( result + 1 ))
  fi
}

for ((i=0; ${i}<40000000; i++)); do
  iterate1
done

echo "Part 1: ${result}"

a_prev=$(cat inputs/input.txt | grep "Generator A" | cut -d " " -f 5)
b_prev=$(cat inputs/input.txt | grep "Generator B" | cut -d " " -f 5)
result=0

function iterate2() {
  a_prev=$(( (a_prev * a_factor) % remainder ))
  while (( ${a_prev} % ${a_mod} > 0 )); do
    a_prev=$(( (a_prev * a_factor) % remainder ))
  done
  b_prev=$(( (b_prev * b_factor) % remainder ))
  while (( ${b_prev} % ${b_mod} > 0 )); do
    b_prev=$(( (b_prev * b_factor) % remainder ))
  done
  local compare=$(( (a_prev % mod) - (b_prev % mod) ))
  if (( ${compare} == 0 )); then
    result=$(( result + 1 ))
  fi
}

for ((i=0; ${i}<5000000; i++)); do
  iterate2
done

echo "Part 2: ${result}"
