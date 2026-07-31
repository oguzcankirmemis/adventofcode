#!/bin/bash

IFS="," read -a instructions < inputs/input.txt

indexes=("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15")
mappings=("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15")
programs=("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15")

function spin () {
  local x=${1}
  local length=${#programs[@]}
  local start=$(( length - x ))
  indexes=("${indexes[@]:start}" "${indexes[@]:0:start}")
}

function exchange () {
  local x=${1}
  local y=${2}
  t=${indexes[${x}]}
  indexes[${x}]=${indexes[${y}]}
  indexes[${y}]=${t}
}

function find_mapping () {
  local x=${1}
  local i=""
  for ((i=0; ${i}<${#mappings[@]}; i++)); do
    if [[ "${mappings[${i}]}" = "${x}" ]]; then
      printf "%s" "${i}"
      return 0
    fi
  done
}

function partner () {
  local x=${1}
  local y=${2}
  x=$(printf "%d" "'${x}")
  x=$(( x - 97 ))
  y=$(printf "%d" "'${y}")
  y=$(( y - 97 ))
  local t=${mappings[${x}]}
  mappings[${x}]=${mappings[${y}]}
  mappings[${y}]=${t}
}

for instruction in "${instructions[@]}"; do
  if [[ "${instruction:0:1}" = "s" ]]; then
    spin "${instruction:1}"
  elif [[ "${instruction:0:1}" = "x" ]]; then
    exchange "$(cut -d / -f 1 <<< ${instruction:1})" "$(cut -d / -f 2 <<< ${instruction:1})"
  else
    partner "$(cut -d / -f 1 <<< ${instruction:1})" "$(cut -d / -f 2 <<< ${instruction:1})"
  fi
done

function iterate () {
  local new_programs=()
  local i=""
  for ((i=0; ${i}<${#programs[@]}; i++)); do
    local idx=${indexes[${i}]}
    idx=$(find_mapping "${programs[${idx}]}")
    new_programs[${i}]=${idx}
  done
  programs=("${new_programs[@]}")
}

function square () {
  local new_indexes=()
  local i=""
  local idx=""
  for ((i=0; ${i}<${#indexes[@]}; i++)); do
    idx=${indexes[${i}]}
    idx=${indexes[${idx}]}
    new_indexes[${i}]=${idx}
  done
  indexes=("${new_indexes[@]}")
  local new_mappings=()
  for ((i=0; ${i}<${#mappings[@]}; i++)); do
    idx=${mappings[${i}]}
    idx=${mappings[${idx}]}
    new_mappings[${i}]=${idx}
  done
  mappings=("${new_mappings[@]}")
}

function convert () {
  local string=""
  local i=""
  for ((i=0; ${i}<${#programs[@]}; i++)); do
    local char=$(( programs[i] + 97 ))
    char="\\$(printf '%03o' ${char})"
    char=$(printf "${char}")
    string="${string}${char}"
  done
  printf "%s" "${string}"
}

iterate

result=$(convert)

echo "Part 1: ${result}"

iterations=999999999

while (( ${iterations} > 0 )); do
  if (( ${iterations} % 2 == 1 )); then
    iterate
  fi
  square
  iterations=$(( iterations / 2 ))
done

result=$(convert)

echo "Part 2: ${result}"
