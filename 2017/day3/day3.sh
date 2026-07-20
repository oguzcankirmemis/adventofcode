#!/bin/bash

input=$(cat inputs/input.txt)
square_length=1
dirs_x=(0 -1 0 1)
dirs_y=(1 0 -1 0)
dirs_length=${#dirs_x[@]}

function abs () {
  local num=${1}
  echo $(( num < 0 ? -num : num ))
}

function index () {
  local x=${1}
  local y=${2}
  local abs_x=$(abs ${x})
  local abs_y=$(abs ${y})
  local length=$(( abs_x > abs_y ? 2 * abs_x + 1 : 2 * abs_y + 1 ))
  if [[ "${length}" == "1" ]]; then
    echo "0"
    return 0
  fi
  x=$(( x + length / 2 ))
  y=$(( y + length / 2 ))
  local index=$(( (length - 2) * (length - 2) - 1 ))
  if [[ "${y}" == "0" ]]; then
    index=$(( index + 3 * length - 3  + x ))
    echo "${index}"
    return 0
  fi
  if [[ "${x}" == "$(( length - 1 ))" ]]; then
    index=$(( index + y ))
    echo "${index}"
    return 0
  fi
  if [[ "${y}" == "$(( length - 1 ))" ]]; then
    index=$(( index + length - 1 + length - 1 - x ))
    echo "${index}"
    return 0
  fi
  if [[ "${x}" == "0" ]]; then
    index=$(( index + 2 * length - 2 + length - 1 - y ))
    echo "${index}"
    return 0
  fi
  echo "failure in calculating the index"
  return 1
}

function score () {
  local x=${1}
  local y=${2}
  if [[ "${x}" == "0" ]] && [[ "${y}" == "0" ]]; then
    echo "1"
    return 0
  fi
  local dirs_x=(1 1 0 -1 -1 -1 0 1)
  local dirs_y=(0 1 1 1 0 -1 -1 -1)
  local score=0
  for i in {0..7}; do
    local n_x=$(( x + dirs_x[i] ))
    local n_y=$(( y + dirs_y[i] ))
    local n_i=$(index n_x n_y)
    if (( ${n_i} < ${#scores[@]} )); then
      score=$(( score + scores[n_i] ))
    fi
  done
  echo "${score}"
  return 0
}

while (( ${square_length} * ${square_length} < ${input} )); do
  square_length=$(( square_length + 2 ))
done

x=$(( square_length / 2 ))
y=$(( 1 - (square_length / 2) ))
current=$(( (square_length - 2) * (square_length - 2) + 1 ))
dir=0

while [[ "${current}" != "${input}" ]]; do
  x=$(( x + dirs_x[dir] ))
  y=$(( y + dirs_y[dir] ))
  if [[ "$(abs x)" == "$(( square_length / 2 ))" ]] && [[ "$(abs y)" == "$(( square_length / 2 ))" ]]; then
    dir=$(( dir + 1 ))
  fi
  current=$(( current + 1 ))
done

distance_x=$(abs x)
distance_y=$(abs y)
distance=$(( distance_x + distance_y ))

echo "Part 1: ${distance}"

dirs_x=(1 0 -1 0)
dirs_y=(0 1 0 -1)

scores=()
last_score=0
square_length=1
x=0
y=0
dir=0

while (( ${last_score} < ${input} )); do
  last_score=$(score ${x} ${y})
  scores[${#scores[@]}]=${last_score}
  x=$(( x + dirs_x[dir] ))
  y=$(( y + dirs_y[dir] ))
  if [[ "$(abs x)" == "$(( (square_length + 2) / 2 ))" ]] || [[ "$(abs y)" == "$(( (square_length + 2) / 2 ))" ]]; then
    dir=$(( (dir + 1) % dirs_length ))
    square_length=$(( square_length + 2 ))
  elif [[ "$(abs x)" == "$(( square_length / 2 ))" ]] && [[ "$(abs y)" == "$(( square_length / 2 ))" ]]; then
    if [[ "${x}" != "$(( square_length / 2 ))" ]] || [[ "$(( -y ))" != "$(( square_length / 2 ))" ]]; then
      dir=$(( (dir + 1) % dirs_length ))
    fi
  fi
done

echo "Part 2: ${last_score}"
