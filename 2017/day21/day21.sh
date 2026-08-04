#!/bin/bash

pattern=()
map2=()
map3=()
naive_map2=()
naive_map3=()
fractal=(".#." "..#" "###")
current=()
new_current=()

function flip () {
  local len=${#pattern[@]}
  local new_pattern=()
  local i=""
  local j=""
  for ((i=0; ${i}<${len}; i++)); do
    local str=${new_pattern[i]}
    local idx=$(( len - i - 1 ))
    for ((j=0; ${j}<${len}; j++)); do
      str="${str}${pattern[idx]:j:1}"
    done
    new_pattern[${i}]="${str}"
  done
  pattern=("${new_pattern[@]}")
}

function rotate () {
  local len=${#pattern[@]}
  local new_pattern=()
  local i=""
  local j=""
  for ((i=0; ${i}<${len}; i++)); do
    local str=${new_pattern[i]}
    for ((j=0; ${j}<${len}; j++)); do
      local idx=$(( len - j - 1 ))
      str="${str}${pattern[idx]:i:1}"
    done
    new_pattern[${i}]="${str}"
  done
  pattern=("${new_pattern[@]}")
}

function parse2 {
  local input="${1}"
  local output="${2}"
  local naive_output="${2}"
  IFS="/" read -a pattern <<< "${output}"
  output=$(hash)
  IFS="/" read -a pattern <<< "${input}"
  map2[$(hash)]="${output}"
  naive_map2[$(hash)]="${naive_output}"
  rotate
  map2[$(hash)]="${output}"
  naive_map2[$(hash)]="${naive_output}"
  rotate
  map2[$(hash)]="${output}"
  naive_map2[$(hash)]="${naive_output}"
  rotate
  map2[$(hash)]="${output}"
  naive_map2[$(hash)]="${naive_output}"
  flip
  map2[$(hash)]="${output}"
  naive_map2[$(hash)]="${naive_output}"
  rotate
  map2[$(hash)]="${output}"
  naive_map2[$(hash)]="${naive_output}"
  rotate
  map2[$(hash)]="${output}"
  naive_map2[$(hash)]="${naive_output}"
  rotate
  map2[$(hash)]="${output}"
  naive_map2[$(hash)]="${naive_output}"
}

function parse3 () {
  local input="${1}"
  local output="${2}"
  local naive_output="${2}"
  local big_pattern=()
  IFS="/" read -a big_pattern <<< "${output}"
  pattern=("${big_pattern[0]:0:2}" "${big_pattern[1]:0:2}")
  output="$(hash)"
  pattern=("${big_pattern[0]:2:2}" "${big_pattern[1]:2:2}")
  output="${output}#$(hash)"
  pattern=("${big_pattern[2]:0:2}" "${big_pattern[3]:0:2}")
  output="${output}#$(hash)"
  pattern=("${big_pattern[2]:2:2}" "${big_pattern[3]:2:2}")
  output="${output}#$(hash)"
  IFS="/" read -a pattern <<< "${input}"
  map3[$(hash)]="${output}"
  naive_map3[$(hash)]="${naive_output}"
  rotate
  map3[$(hash)]="${output}"
  naive_map3[$(hash)]="${naive_output}"
  rotate
  map3[$(hash)]="${output}"
  naive_map3[$(hash)]="${naive_output}"
  rotate
  map3[$(hash)]="${output}"
  naive_map3[$(hash)]="${naive_output}"
  flip
  map3[$(hash)]="${output}"
  naive_map3[$(hash)]="${naive_output}"
  rotate
  map3[$(hash)]="${output}"
  naive_map3[$(hash)]="${naive_output}"
  rotate
  map3[$(hash)]="${output}"
  naive_map3[$(hash)]="${naive_output}"
  rotate
  map3[$(hash)]="${output}"
  naive_map3[$(hash)]="${naive_output}"
}

function hash () {
  local i=""
  local j=""
  local sum=0
  local base=1
  for ((i=0; ${i}<${#pattern[@]}; i++)); do
    local str=${pattern[i]}
    for ((j=0; ${j}<${#pattern[@]}; j++)); do
      if [[ "${str:j:1}" = "#" ]]; then
        sum=$(( sum + base ))
      fi
      base=$(( 2 * base ))
    done
  done
  printf "%s" "${sum}"
}

function naive_iterate_even () {
  local new_fractal=()
  local i=""
  local j=""
  local k=0
  for ((i=0; ${i}<${#fractal[@]}; i=i+2)); do
    for ((j=0; ${j}<${#fractal[@]}; j=j+2)); do
      pattern=("${fractal[i]:j:2}" "${fractal[i+1]:j:2}")
      local out="${naive_map2[$(hash)]}"
      local str="${new_fractal[k]}"
      new_fractal[${k}]="${str}$(cut -d '/' -f 1 <<< ${out})"
      str="${new_fractal[k+1]}"
      new_fractal[$(( k + 1 ))]="${str}$(cut -d '/' -f 2 <<< ${out})"
      str="${new_fractal[k+2]}"
      new_fractal[$(( k + 2 ))]="${str}$(cut -d '/' -f 3 <<< ${out})"
    done
    k=$(( k + 3 ))
  done
  fractal=("${new_fractal[@]}")
}

function naive_iterate_odd () {
  local new_fractal=()
  local i=""
  local j=""
  local k=0
  for ((i=0; ${i}<${#fractal[@]}; i=i+3)); do
    for ((j=0; ${j}<${#fractal[@]}; j=j+3)); do
      pattern=("${fractal[i]:j:3}" "${fractal[i+1]:j:3}" "${fractal[i+2]:j:3}")
      local out="${naive_map3[$(hash)]}"
      local str="${new_fractal[k]}"
      new_fractal[${k}]="${str}$(cut -d '/' -f 1 <<< ${out})"
      str="${new_fractal[k+1]}"
      new_fractal[$(( k + 1 ))]="${str}$(cut -d '/' -f 2 <<< ${out})"
      str="${new_fractal[k+2]}"
      new_fractal[$(( k + 2 ))]="${str}$(cut -d '/' -f 3 <<< ${out})"
      str="${new_fractal[k+3]}"
      new_fractal[$(( k + 3 ))]="${str}$(cut -d '/' -f 4 <<< ${out})"
    done
    k=$(( k + 4 ))
  done
  fractal=("${new_fractal[@]}")
}

function naive_iterate () {
  if (( ${#fractal[@]} % 2 == 0 )); then
    naive_iterate_even
  else
    naive_iterate_odd
  fi
}

function convert_hash () {
  local h=${1}
  local i=""
  local j=""
  fractal=()
  for ((i=0; ${i}<3; i++)); do
    for ((j=0; ${j}<3; j++)); do
      if (( ${h} % 2 == 1 )); then
        fractal[${i}]="${fractal[i]}#"
      else
        fractal[${i}]="${fractal[i]}."
      fi
      h=$(( h / 2 ))
    done
  done
}

function divide () {
  local count=${1}
  local i=""
  local j=""
  for ((i=0; ${i}<${#fractal[@]}; i=i+3)); do
    for ((j=0; ${j}<${#fractal[@]}; j=j+3)); do
      pattern=("${fractal[i]:j:3}" "${fractal[i+1]:j:3}" "${fractal[i+2]:j:3}")
      local h=$(hash)
      if [[ "${new_current[h]}" = "" ]]; then
        new_current[${h}]="${h}#0"
      fi
      local c=$(cut -d "#" -f 2 <<< "${new_current[h]}")
      new_current[${h}]="${h}#$(( c + count ))"
    done
  done
}

function iterate () {
  new_current=()
  for p in "${current[@]}"; do
    local h=$(cut -d "#" -f 1 <<< "${p}")
    local c=$(cut -d "#" -f 2 <<< "${p}")
    convert_hash "${h}"
    naive_iterate
    naive_iterate
    naive_iterate
    divide "${c}"
  done
  current=("${new_current[@]}")
}

function naive_count () {
  local total=0
  local i=""
  for ((i=0; ${i}<${#fractal[@]}; i++)); do
    local partial=$(printf "%s" "${fractal[i]}" | tr -d "." | wc -m)
    total=$(( total + partial ))
  done
  printf "%s" "${total}"
}

function count () {
  local total=0
  for p in "${current[@]}"; do
    local t=0
    local h=$(cut -d "#" -f 1 <<< "${p}")
    local c=$(cut -d "#" -f 2 <<< "${p}")
    while (( ${h} > 0 )); do
      if (( ${h} % 2 == 1 )); then
        t=$(( t + 1 ))
      fi
      h=$(( h / 2 ))
    done
    total=$(( total + c * t ))
  done
  printf "%s" "${total}"
}

while read input ign output; do
  if (( ${#input} < 9 )); then
    parse2 "${input}" "${output}"
  else
    parse3 "${input}" "${output}"
  fi
done < inputs/input.txt

for ((i=0; ${i}<5; i++)); do
  naive_iterate
done

echo "Part 1: $(naive_count)"

naive_iterate
divide 1
current=("${new_current[@]}")

for ((i=0; ${i}<4; i++)); do
  iterate
done

echo "Part 2: $(count)"
