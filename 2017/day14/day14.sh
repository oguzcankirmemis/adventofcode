#!/bin/bash

function hash () {
  local input="${1}"
  local rounds=64
  local index=0
  local skip=0
  local size=256
  local sequence=()
  local list=()
  local length=""
  local i=""
  local j=""
  local t=""
  local block=""
  local binary=""
  local result=""
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
    local length=""
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
    binary=$(echo "obase=2; ${block}" | bc)
    binary=$(printf "%08d" "${binary}")
    result="${result}${binary}"
  done
  printf "%s" "${result}"
}

map=""
rows=128
key=$(cat inputs/input.txt)

for ((i=0; ${i}<${rows}; i++)); do
  string="${key}-${i}"
  hashed=$(hash "${string}")
  map="${map}${hashed}"
done

result=$(printf "%s" "${map}" | tr -d "0" | wc -m)

echo "Part 1: ${result}"

parents=()
ranks=()

function find () {
  local x=${1}
  if [[ "${parents[${x}]}" = "" ]]; then
    parents[${x}]=${x}
    ranks[${x}]=1
  fi
  if (( ${parents[${x}]} != ${x} )); then
    local new_parent=${parents[${x}]}
    parents[${x}]=${parents[${new_parent}]}
    find "${parents[${x}]}"
  fi
}

function union () {
  local x=${1}
  local y=${2}
  if [[ "${parents[${y}]}" = "" ]]; then
    parents[${y}]=${y}
    ranks[${y}]=1
  fi
  find "${x}"
  x=${parents[${x}]}
  find "${y}"
  y=${parents[${y}]}
  if (( ${x} == ${y} )); then
    return 0
  fi
  if (( ${ranks[${x}]} < ${ranks[${y}]} )); then
    local t=${x}
    x=${y}
    y=${t}
  fi
  parents[${y}]=${x}
  if (( ${ranks[${x}]} == ${ranks[${y}]} )); then
    ranks[${x}]=$(( ranks[x] + 1 ))
  fi
}

result=0

for ((i=0; ${i}<${rows}; i++)); do
  for ((j=0; ${j}<${rows}; j++)); do
    idx1=$(( j + i * rows ))
    if [[ "${map:${idx1}:1}" = "0" ]]; then
      continue
    fi
    find "${idx1}"
    if (( ${i} + 1 < ${rows} )); then
      idx2=$(( j + (i + 1) * rows ))
      if [[ "${map:${idx2}:1}" = "1" ]]; then
        union "${idx1}" "${idx2}"
      fi
    fi
    if (( ${j} + 1 < ${rows} )); then
      idx2=$(( j + 1 + i * rows ))
      if [[ "${map:${idx2}:1}" = "1" ]]; then
        union "${idx1}" "${idx2}"
      fi
    fi
  done
done

hashset=()

for parent in "${parents[@]}"; do
  if (( ${parent} == ${parents[${parent}]} )) && [[ "${hashset[${parent}]}" = "" ]]; then
    result=$(( result + 1 ))
    hashset[${parent}]="counted"
  fi
done

echo "Part 2: ${result}"
