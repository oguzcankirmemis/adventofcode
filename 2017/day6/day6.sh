#!/bin/bash

read -a banks < inputs/input.txt
length=${#banks[@]}

function max () {
  local max=-1
  local max_i=-1
  local i=0
  for bank in ${banks[@]}; do
    if (( ${bank} > ${max} )); then
      max=${bank}
      max_i=${i}
    fi
    i=$(( i + 1 ))
  done
  echo "${max_i}"
}

record=""
hash="${banks[@]}"
steps=0
while ! grep -q "${hash}" <<< "${record}"; do
  record="${record}
${banks[@]}"
  i=$(max)
  memory=${banks[${i}]}
  banks[${i}]=0
  i=$(( (i + 1) % length ))
  while (( memory > 0 )); do
    banks[${i}]=$(( banks[i] + 1 ))
    memory=$(( memory - 1 ))
    i=$(( (i + 1) % length ))
  done
  hash="${banks[@]}"
  steps=$(( steps + 1 ))
done

echo "Part 1: ${steps}"

lines=$(wc -l <<< "${record}")
occurs_at=$(grep -n "${hash}" <<< "${record}" | cut -d ":" -f 1)
repeat=$(( lines - occurs_at + 1 ))

echo "Part 2: ${repeat}"
