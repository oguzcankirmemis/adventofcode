#!/bin/bash

depths=()
scanners=()
dirs=$()
max_layer=0

layers=()
cycles=()

while read layer_with_suffix range; do
  layer=$(tr -d ":" <<< "${layer_with_suffix}")
  depths[${layer}]=${range}
  scanners[${layer}]=0
  dirs[${layer}]=1
  if (( ${layer} > max_layer )); then
    max_layer=${layer}
  fi
  layers[${#layers[@]}]=${layer}
  cycles[${#cycles[@]}]=$(( (range - 1) * 2 ))
done < inputs/input.txt

player=-1
severity=0

function simulate () {
  player=$(( player + 1 ))
  if [[ "${scanners[${player}]}" != "" ]] && (( ${scanners[${player}]} == 0 )); then
    severity=$(( severity + (player * depths[player]) ))
  fi
  local scanner=""
  for ((scanner=0; ${scanner}<=${max_layer}; scanner++)); do
    if [[ "${scanners[${scanner}]}" = "" ]] || (( ${depths[${scanner}]} == 1 )); then
      continue
    fi
    scanners[${scanner}]=$(( scanners[scanner] + dirs[scanner] ))
    if (( ${scanners[${scanner}]} < 0 )); then
      scanners[${scanner}]=1
      dirs[${scanner}]=1
    elif (( ${scanners[${scanner}]} >= ${depths[${scanner}]} )); then
      scanners[${scanner}]=$(( depths[scanner] - 2 ))
      dirs[${scanner}]=-1
    fi
  done
}

function check_time () {
  local start=${1}
  local i=""
  for ((i=0; ${i}<${#cycles[@]}; i++)); do
    local arrival=$(( start + layers[i] ))
    local remainder=$(( arrival % cycles[i] ))
    if (( ${remainder} == 0 )); then
      return 1
    fi
  done
  return 0
}

while (( ${player} <= ${max_layer} )); do
  simulate
done

echo "Part 1: ${severity}"

min_time=0

while ! check_time "${min_time}"; do
  min_time=$(( min_time + 1 ))
done

echo "Part 2: ${min_time}"
