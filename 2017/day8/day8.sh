#!/bin/bash

function hash () {
  local str=${1}
  local size=${2}
  local sum=0
  local c=""
  for c in $(sed 's/./& /g' <<< "${str}"); do
    local code=$(printf "%d" "'${c}")
    sum=$(( (27 * sum) + code ))
  done
  local hash=$(( sum  % size ))
  echo "${hash}"
}

str_map=()
map=()
size=10000

moment_max=0
while read r1 op imm1 ign r2 cond imm2; do
  hash_r1=$(hash ${r1} ${size})
  hash_r2=$(hash ${r2} ${size})
  if [[ "${map[${hash_r1}]}" = "" ]]; then
    str_map[${hash_r1}]=${r1}
    map[${hash_r1}]=0
  elif [[ "${str_map[${hash_r1}]}" != "${r1}" ]]; then
    echo "Conflict - ${r1} - ${str_map[${hash_r1}]} - ${hash_r1}" 1>&2
  fi
  if [[ "${map[${hash_r2}]}" = "" ]]; then
    str_map[${hash_r2}]=${r2}
    map[${hash_r2}]=0
  elif [[ "${str_map[${hash_r2}]}" != "${r2}" ]]; then
    echo "Conflict - ${r2} - ${str_map[${hash_r2}]} - ${hash_r2}" 1>&2
  fi
  if (( ${map[${hash_r2}]} ${cond} ${imm2} )); then
    prev=${map[${hash_r1}]}
    if [[ "${op}" = "inc" ]]; then
      map[${hash_r1}]=$(( prev + imm1 ))
    fi
    if [[ "${op}" = "dec" ]]; then
      map[${hash_r1}]=$(( prev - imm1 ))
    fi
  fi
  if (( ${map[${hash_r1}]} > ${moment_max} )); then
    moment_max=${map[${hash_r1}]}
  fi
done < inputs/input.txt

max=-999999999999
for el in ${map[@]}; do
  if [[ "${el}" != "" ]] && (( ${el} > ${max} )); then
    max=${el}
  fi
done

echo "Part 1: ${max}"
echo "Part 2: ${moment_max}"

