#!/bin/bash

p_x=()
p_y=()
p_z=()

v_x=()
v_y=()
v_z=()

a_x=()
a_y=()
a_z=()

out=()
destroyed=()

while read p v a; do
  p=$(cut -d "<" -f 2 <<< "${p}" | cut -d ">" -f 1)
  v=$(cut -d "<" -f 2 <<< "${v}" | cut -d ">" -f 1)
  a=$(cut -d "<" -f 2 <<< "${a}" | cut -d ">" -f 1)
  IFS="," read x y z <<< "${p}"
  p_x[${#p_x[@]}]=${x}
  p_y[${#p_y[@]}]=${y}
  p_z[${#p_z[@]}]=${z}
  IFS="," read x y z <<< "${v}"
  v_x[${#v_x[@]}]=${x}
  v_y[${#v_y[@]}]=${y}
  v_z[${#v_z[@]}]=${z}
  IFS="," read x y z <<< "${a}"
  a_x[${#a_x[@]}]=${x}
  a_y[${#a_y[@]}]=${y}
  a_z[${#a_z[@]}]=${z}
done < inputs/input.txt

len=${#p_x[@]}

function distance_p () {
  local i=${1}
  local d=0
  if (( ${p_x[${i}]} < 0 )); then
    d=$(( d - p_x[i] ))
  else
    d=$(( d + p_x[i] ))
  fi
  if (( ${p_y[${i}]} < 0 )); then
    d=$(( d - p_y[i] ))
  else
    d=$(( d + p_y[i] ))
  fi
  if (( ${p_z[${i}]} < 0 )); then
    d=$(( d - p_z[i] ))
  else
    d=$(( d + p_z[i] ))
  fi
  printf "%s" "${d}"
}

function distance_v () {
  local i=${1}
  local d=0
  if (( ${v_x[${i}]} < 0 )); then
    d=$(( d - v_x[i] ))
  else
    d=$(( d + v_x[i] ))
  fi
  if (( ${v_y[${i}]} < 0 )); then
    d=$(( d - v_y[i] ))
  else
    d=$(( d + v_y[i] ))
  fi
  if (( ${v_z[${i}]} < 0 )); then
    d=$(( d - v_z[i] ))
  else
    d=$(( d + v_z[i] ))
  fi
  printf "%s" "${d}"
}

function distance_a () {
  local i=${1}
  local d=0
  if (( ${a_x[${i}]} < 0 )); then
    d=$(( d - a_x[i] ))
  else
    d=$(( d + a_x[i] ))
  fi
  if (( ${a_y[${i}]} < 0 )); then
    d=$(( d - a_y[i] ))
  else
    d=$(( d + a_y[i] ))
  fi
  if (( ${a_z[${i}]} < 0 )); then
    d=$(( d - a_z[i] ))
  else
    d=$(( d + a_z[i] ))
  fi
  printf "%s" "${d}"
}

smallest=0
smallest_p_d=$(distance_p 0)
smallest_v_d=$(distance_v 0)
smallest_a_d=$(distance_a 0)

for ((i=1; ${i}<${len}; i++)); do
  p_d=$(distance_p "${i}")
  v_d=$(distance_v "${i}")
  a_d=$(distance_a "${i}")
  smaller="false"
  if (( ${a_d} < ${smallest_a_d} )); then
    smaller="true"
  elif (( ${a_d} == ${smallest_a_d} )) &&
        (( ${v_d} < ${smallest_v_d} )); then
    smaller="true"
  elif (( ${a_d} == ${smallest_a_d} )) &&
        (( ${v_d} == ${smallest_v_d} )) &&
        (( ${p_d} < ${smallest_p_d} )); then
    smaller="true"
  fi
  if [[ "${smaller}" = "true" ]]; then
    smallest=${i}
    smallest_p_d=${p_d}
    smallest_v_d=${v_d}
    smallest_a_d=${a_d}
  fi
done

echo "Part 1: ${smallest}"

left=${len}

function collision () {
  local i=${1}
  local j=${2}
  if (( ${p_x[${i}]} == ${p_x[${j}]} )) &&
      (( ${p_y[${i}]} == ${p_y[${j}]} )) &&
      (( ${p_z[${i}]} == ${p_z[${j}]} )); then
    return 0
  fi
  return 1
}

function check_collision () {
  local i=""
  local j=""
  for ((i=0; ${i}<${len}; i++)); do
    if [[ "${destroyed[${i}]}" = "true" ]]; then
      continue
    fi
    local colliding=0
    for ((j=$(( i + 1 )); ${j}<${len}; j++)); do
      if [[ "${destroyed[${j}]}" = "true" ]]; then
        continue
      fi
      if collision "${i}" "${j}"; then
        destroyed[${i}]="true"
        destroyed[${j}]="true"
        colliding=$(( colliding + 1 ))
      fi
    done
    if (( ${colliding} > 0 )); then
      left=$(( left - colliding - 1 ))
    fi
  done
}

function simulate () {
  local i=""
  for ((i=0; ${i}<${len}; i++)); do
    if [[ "${destroyed[${i}]}" = "true" ]]; then
      continue
    fi
    v_x[${i}]=$(( v_x[i] + a_x[i] ))
    v_y[${i}]=$(( v_y[i] + a_y[i] ))
    v_z[${i}]=$(( v_z[i] + a_z[i] ))
    p_x[${i}]=$(( p_x[i] + v_x[i] ))
    p_y[${i}]=$(( p_y[i] + v_y[i] ))
    p_z[${i}]=$(( p_z[i] + v_z[i] ))
  done
  check_collision
}

function collide_time_x () {
  local i=${1}
  local j=${2}
  local da_x=$(( a_x[i] - a_x[j] ))
  local dv_x=$(( 2 * v_x[i] - 2 * v_x[j] + a_x[i] - a_x[j] ))
  local dp_x=$(( 2 * p_x[i] - 2 * p_x[j] ))
  local sol=""
  if (( ${da_x} == 0 )); then
    if (( ${dv_x} == 0 )); then
      printf "%s" "-1"
      return 1
    fi
    sol=$(( -dp_x / dv_x ))
    if (( ${sol} * ${dv_x} == -${dp_x} )); then
      printf "%s" "${sol}"
      return 0
    fi
    printf "%s" "-1"
    return 1
  fi
  local square=$(( dv_x * dv_x - 4 * da_x * dp_x ))
  if (( ${square} < 0 )); then
    printf "%s" "-1"
    return 1
  fi
  local root=$(bc <<< "sqrt(${square})")
  if (( ${root} * ${root} != ${square} )); then
    printf "%s" "-1"
    return 1
  fi
  local sol1=$(( (-dv_x - square) / (2 * da_x) ))
  if (( ${sol1} >= 0 )) && (( 2 * ${sol1} * ${da_x} == -${dv_x} - ${square} )); then
    sol="${sol1}"
  fi
  local sol2=$(( (-dv_x + square) / (2 * da_x) ))
  if (( ${sol2} >= 0 )) && (( 2 * ${sol2} * ${da_x} == -${dv_x} + ${square} )); then
    if [[ "${sol}" = "" ]]; then
      sol="${sol2}"
    else
      sol="${sol},${sol2}"
    fi
  fi
  if [[ "${sol}" = "" ]]; then
    printf "%s" -1
    return 1
  fi
  printf "%s" "${sol}"
  return 0
}

function collide_time () {
  local i=${1}
  local j=${2}
  local dt1=$(collide_time_x ${i} ${j})
  local dt2=""
  if grep -s "," <<< "${d1}"; then
    dt2=$(cut -d "," -f 2 <<< "${dt1}")
    dt1=$(cut -d "," -f 1 <<< "${dt1}")
  fi
  if (( ${dt1} == -1 )); then
    printf "%s" "-1"
    return 1
  fi
  local y_i=$(( (2 * p_y[i] + 2 * dt1 * v_y[i] + dt1 * a_y[i] + dt1 * dt1 * a_y[i]) / 2 ))
  local y_j=$(( (2 * p_y[j] + 2 * dt1 * v_y[j] + dt1 * a_y[j] + dt1 * dt1 * a_y[j]) / 2 ))
  local z_i=$(( (2 * p_z[i] + 2 * dt1 * v_z[i] + dt1 * a_z[i] + dt1 * dt1 * a_z[i]) / 2 ))
  local z_j=$(( (2 * p_z[j] + 2 * dt1 * v_z[j] + dt1 * a_z[j] + dt1 * dt1 * a_z[j]) / 2 ))
  if (( ${y_i} == ${y_j} )) && (( ${z_i} == ${z_j} )); then
    printf "%s" "${dt1}"
    return 0
  fi
  if [[ "${dt2}" = "" ]]; then
    printf "%s" "-1"
    return 1
  fi
  local y_i=$(( (2 * p_y[i] + 2 * dt2 * v_y[i] + dt2 * a_y[i] + dt2 * dt2 * a_y[i]) / 2 ))
  local y_j=$(( (2 * p_y[j] + 2 * dt2 * v_y[j] + dt2 * a_y[j] + dt2 * dt2 * a_y[j]) / 2 ))
  local z_i=$(( (2 * p_z[i] + 2 * dt2 * v_z[i] + dt2 * a_z[i] + dt2 * dt2 * a_z[i]) / 2 ))
  local z_j=$(( (2 * p_z[j] + 2 * dt2 * v_z[j] + dt2 * a_z[j] + dt2 * dt2 * a_z[j]) / 2 ))
  if (( ${y_i} == ${y_j} )) && (( ${z_i} == ${z_j} )); then
    printf "%s" "${dt2}"
    return 0
  fi
  printf "%s" "-1"
  return 1
}

current_time=0

for ((i=0; ${i}<${len}; i++)); do
  if [[ "${destroyed[${i}]}" = "true" ]]; then
    continue
  fi
  for ((j=$(( i + 1 )); ${j}<${len}; j++)); do
    if [[ "${destroyed[${j}]}" = "true" ]]; then
      continue
    fi
    t=$(collide_time ${i} ${j})
    while (( ${t} > 0 )); do
      simulate
      current_time=$(( current_time + 1 ))
      t=$(( t - 1 ))
    done
  done
done

echo "Part 2: ${left}"
