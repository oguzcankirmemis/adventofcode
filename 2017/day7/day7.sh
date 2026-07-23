#!/bin/bash

while read line; do
  node=$(cut -d " " -f 1 <<< "${line}")
  if ! grep -q "\-> .*${node}" inputs/input.txt; then
    break
  fi
done < inputs/input.txt

echo "Part 1: ${node}"

function compute_weight () {
  local node=${1}
  local line=$(grep "${node} (" inputs/input.txt)
  local weight=$(cut -d "(" -f 2 <<< "${line}" | cut -d ")" -f 1)
  local children=$(cut -d ">" -f 2 <<< "${line}")
  if grep -q "(" <<< "${children}"; then
    echo "${weight}"
    return
  fi
  local child=""
  IFS="," read -a children <<< "${children}"
  local bucket1_weight=-1
  local bucket1_count=0
  local bucket1_last=""
  local bucket2_weight=-1
  local bucket2_count=0
  local bucket2_last=""
  for child in ${children[@]}; do
    child_weight=$(compute_weight ${child})
    if [[ -f adjusted_weight ]]; then
      return
    fi
    if (( ${bucket1_weight} == -1 || ${bucket1_weight} == ${child_weight} )); then
      bucket1_weight=${child_weight}
      bucket1_count=$(( bucket1_count + 1 ))
      bucket1_last=${child}
    else
      bucket2_weight=${child_weight}
      bucket2_count=$(( bucket2_count + 1 ))
      bucket2_last=${child}
    fi
    weight=$(( weight + child_weight ))
  done
  if (( ${bucket2_weight} != -1 )); then
    local diff=0
    local diff_node=""
    if (( ${bucket1_count} == 1 )); then
      diff=$(( bucket2_weight - bucket1_weight ))
      diff_node=${bucket1_last}
    else
      diff=$(( bucket1_weight - bucket2_weight ))
      diff_node=${bucket2_last}
    fi
    local diff_node_weight=$(grep "${diff_node} (" inputs/input.txt | cut -d "(" -f 2 | cut -d ")" -f 1)
    local adjusted_weight=$(( diff_node_weight + diff ))
    echo "${adjusted_weight}" > adjusted_weight
    return
  fi
  echo "${weight}"
}

compute_weight ${node} > /dev/null
adjusted_weight=$(cat adjusted_weight)
rm adjusted_weight

echo "Part 2: ${adjusted_weight}"
