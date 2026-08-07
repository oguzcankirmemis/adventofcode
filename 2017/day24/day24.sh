#!/bin/bash

hash_base=1000000
sockets=()

while IFS="/" read start end; do
  if [[ "${sockets[start]}" = "" ]]; then
    sockets[${start}]="${end}"
  else
    sockets[${start}]="${sockets[start]}#${end}"
  fi
  if (( ${start} != ${end} )); then
    if [[ "${sockets[end]}" = "" ]]; then
      sockets[${end}]="${start}"
    else
      sockets[${end}]="${sockets[end]}#${start}"
    fi
  fi
done < inputs/input.txt

max_sum=0
max_longest=0
max_longest_sum=0

current_map=()
current_sum=0
current_end=0
current_longest=0

function dfs () {
  if (( ${current_sum} > ${max_sum} )); then
    max_sum=${current_sum}
    echo "${max_sum}"
  fi
  if (( ${current_longest} > ${max_longest} )); then
    max_longest=${current_longest}
    max_longest_sum=${current_sum}
  fi
  if (( ${current_longest} == ${max_longest} )) && (( ${current_sum} > ${max_longest_sum} )); then
    max_longest_sum=${current_sum}
  fi
  local old_end=${current_end}
  local passing_sockets=()
  local passing_socket=""
  IFS="#" read -a passing_sockets <<< "${sockets[current_end]}"
  for passing_socket in "${passing_sockets[@]}"; do
    local hash1=$(( current_end + hash_base * passing_socket ))
    local hash2=$(( hash_base * current_end + passing_socket ))
    if [[ "${current_map[hash1]}" = "" ]] && [[ "${current_map[hash2]}" = "" ]]; then
      current_map[${hash1}]="x"
      current_map[${hash2}]="x"
      current_sum=$(( current_sum + old_end + passing_socket ))
      current_end=${passing_socket}
      current_longest=$(( current_longest + 1 ))
      dfs
      current_map[${hash1}]=""
      current_map[${hash2}]=""
      current_sum=$(( current_sum - old_end - passing_socket ))
      current_end=${old_end}
      current_longest=$(( current_longest - 1 ))
    fi
  done
}

dfs

echo "Part 1: ${max_sum}"
echo "Part 2: ${max_longest_sum}"

