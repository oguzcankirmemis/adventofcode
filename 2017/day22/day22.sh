#!/bin/bash

rm -f v.*.v
input=()

while read line; do
  input[${#input[@]}]="${line}"
done < inputs/input.txt

width=${#input[0]}
height=${#input[@]}

x=$(( width / 2 ))
y=$(( height / 2 ))

infected=0

for ((i=0; ${i}<${height}; i++)); do
  for ((j=0; ${j}<${width}; j++)); do
    if [[ "${input[i]:j:1}" = "#" ]]; then
      touch "v.${i}#${j}.v"
    fi
  done
done

dir=0

function iterate1 () {
  local node="v.${y}#${x}.v"
  if [[ -f "${node}" ]]; then
    dir=$(( (dir + 1) % 4 ))
    rm "${node}"
  else
    dir=$(( dir - 1 ))
    if (( ${dir} < 0 )); then
      dir=3
    fi
    touch "${node}"
    infected=$(( infected + 1 ))
  fi
  if (( ${dir} == 0 )); then
    y=$(( y - 1 ))
  elif (( ${dir} == 1 )); then
    x=$(( x + 1 ))
  elif (( ${dir} == 2 )); then
    y=$(( y + 1 ))
  else
    x=$(( x - 1 ))
  fi
}

for ((i=0; ${i}<10000; i++)); do
  iterate1
done

echo "Part 1: ${infected}"

rm -f v.*.v
infected=0
x=$(( width / 2 ))
y=$(( height / 2 ))
dir=0

for ((i=0; ${i}<${height}; i++)); do
  for ((j=0; ${j}<${width}; j++)); do
    if [[ "${input[i]:j:1}" = "#" ]]; then
      printf "2" > "v.${i}#${j}.v"
    fi
  done
done

function iterate2 () {
  local node="v.${y}#${x}.v"
  if [[ -f "${node}" ]]; then
    state=$(cat "${node}")
    if (( ${state} == 1 )); then
      printf "2" > "${node}"
      infected=$(( infected + 1 ))
    elif (( ${state} == 2 )); then
      printf "3" > "${node}"
      dir=$(( (dir + 1) % 4 ))
    else
      rm "${node}"
      dir=$(( (dir + 2) % 4 ))
    fi
  else
    dir=$(( dir - 1 ))
    if (( ${dir} < 0 )); then
      dir=3
    fi
    printf "1" > "${node}"
  fi
  if (( ${dir} == 0 )); then
    y=$(( y - 1 ))
  elif (( ${dir} == 1 )); then
    x=$(( x + 1 ))
  elif (( ${dir} == 2 )); then
    y=$(( y + 1 ))
  else
    x=$(( x - 1 ))
  fi
}

for ((i=0; ${i}<10000000; i++)); do
  iterate2
done

echo "Part 2 : ${infected}"

rm -f v.*.v
