#!/bin/bash

sizes=()
parents=()

function make_set () {
  local node=""
  local rest=""
  while read node rest; do
    parents[${node}]=${node}
    sizes[${node}]=1
  done < inputs/input.txt
}

function find () {
  local node=${1}
  if (( ${parents[${node}]} != ${node} )); then
    parents[${node}]=$(find "${parents[${node}]}")
    printf "%s" "${parents[${node}]}"
  else
    printf "%s" "${node}"
  fi
}

function union () {
  local node1=${1}
  local node2=${2}
  node1=$(find "${node1}")
  node2=$(find "${node2}")
  if (( ${node1} == ${node2} )); then
    return
  fi
  if (( ${sizes[${node1}]} < ${sizes[${node2}]} )); then
    local tmp=${node1}
    node1=${node2}
    node2=${tmp}
  fi
  parents[${node2}]=${node1}
  sizes[${node1}]=$(( sizes[node1] + sizes[node2] ))
}

make_set
while read node ign neighbours; do
  for neighbour in $(tr -d "," <<< "${neighbours}"); do
    union "${node}" "${neighbour}"
  done
done < inputs/input.txt

result=${sizes[$(find 0)]}

echo "Part 1: ${result}"

groups=0
for ((node=0; node<${#parents[@]}; node++)); do
  if (( ${node} == ${parents[${node}]} )); then
    groups=$(( groups + 1 ))
  fi
done

echo "Part 2: ${groups}"
