#!/bin/bash

step=$(cat inputs/input.txt)
idx=0
buff=("0")

function insert () {
  local item=${1}
  local length=${#buff[@]}
  idx=$(( (idx + step) % length ))
  idx=$(( idx + 1 ))
  if (( ${idx} == ${length} )); then
    buff[${idx}]=${item}
  else
    buff=("${buff[@]:0:idx}" "${item}" "${buff[@]:idx:length}")
  fi
}

for ((i=1; i<=2017; i++)); do
  insert "${i}"
done

length=${#buff[@]}
idx=$(( (idx + 1) % length ))

echo "Part 1: ${buff[idx]}"

element_after_zero=""
idx=0
length=1
zero_idx=0

for ((i=1; ${i}<=50000000; i++)); do
  idx=$(( ((idx + step) % length) + 1 ))
  if (( ${zero_idx} + 1 == ${idx} )); then
    element_after_zero=${i}
  fi
  length=$(( length + 1 ))
  if (( ${i} % 100000 == 0 )); then
    echo "${i}"
  fi
done

echo "Part 2: ${element_after_zero}"
