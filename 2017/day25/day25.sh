#!/bin/bash

input_file="inputs/input.txt"

write0=()
go0=()
next0=()

write1=()
go1=()
next1=()

tape=()
position=0
state=""
steps=0
one_count=0

function parse_state () {
  local state="${1}"
  local state_val=$(printf "%d" "'${state}")
  state_val=$(( state_val - 65 ))
  local state_text=$(cat "${input_file}" | grep -A 8 "In state ${state}:")
  local cond0=$(echo "${state_text}" | grep -A 3 "If the current value is 0:")
  local value0=$(echo "${cond0}" | grep "Write the value" | cut -d " " -f 9 | tr -d ".")
  local dir0=$(echo "${cond0}" | grep "Move one slot to the" | cut -d " " -f 11 | tr -d ".")
  local new_state0=$(echo "${cond0}" | grep "Continue with state" | cut -d " " -f 9 | tr -d ".")
  local new_state0_val=$(printf "%d" "'${new_state0}")
  new_state0_val=$(( new_state0_val - 65 ))
  write0[${state_val}]=${value0}
  go0[${state_val}]=${dir0}
  next0[${state_val}]=${new_state0_val}
  local cond1=$(echo "${state_text}" | grep -A 3 "If the current value is 1:")
  local value1=$(echo "${cond1}" | grep "Write the value" | cut -d " " -f 9 | tr -d ".")
  local dir1=$(echo "${cond1}" | grep "Move one slot to the" | cut -d " " -f 11 | tr -d ".")
  local new_state1=$(echo "${cond1}" | grep "Continue with state" | cut -d " " -f 9 | tr -d ".")
  local new_state1_val=$(printf "%d" "'${new_state1}")
  new_state1_val=$(( new_state1_val - 65 ))
  write1[${state_val}]=${value1}
  go1[${state_val}]=${dir1}
  next1[${state_val}]=${new_state1_val}
}

function parse_states () {
  local ign1=""
  local ign2=""
  local state=""
  while read ign1 ign2 state; do
    state=$(printf "%s" "${state}" | tr -d ":")
    parse_state "${state}"
  done <<< $(cat "${input_file}" | grep "In state")
}

function parse_begin_state () {
  state=$(cat "${input_file}" | grep "Begin in state" | cut -d " " -f 4 | tr -d ".")
  state=$(printf "%d" "'${state}")
  state=$(( state - 65 ))
}

function configure_tape () {
  steps=$(cat "${input_file}" | grep "Perform a diagnostic checksum after" | cut -d " " -f 6)
  position=${steps}
}

function simulate () {
  local current_val="${tape[position]}"
  if [[ "${current_val}" = "" ]]; then
    tape[${position}]=0
    current_val=0
  fi
  if (( ${current_val} == 0 )); then
    local new_val="${write0[state]}"
    if (( ${new_val} == 1 )); then
      one_count=$(( one_count + 1 ))
    fi
    tape[${position}]="${new_val}"
    local dir="${go0[state]}"
    if [[ "${dir}" = "left" ]]; then
      position=$(( position - 1 ))
    elif [[ "${dir}" = "right" ]]; then
      position=$(( position + 1 ))
    else
      echo "Illegal direction in the transition: ${dir}"
    fi
    local new_state="${next0[state]}"
    state="${new_state}"
  elif (( ${current_val} == 1 )); then
    local new_val="${write1[state]}"
    if (( ${new_val} == 0 )); then
      one_count=$(( one_count - 1 ))
    fi
    tape[${position}]="${new_val}"
    local dir="${go1[state]}"
    if [[ "${dir}" = "left" ]]; then
      position=$(( position - 1 ))
    elif [[ "${dir}" = "right" ]]; then
      position=$(( position + 1 ))
    else
      echo "Illegal direction in the transition: ${dir}"
    fi
    local new_state="${next1[state]}"
    state="${new_state}"
  else
    echo "Illegal value in the tape: ${current_val}"
  fi
}

parse_begin_state
configure_tape
parse_states

while (( ${steps} > 0 )); do
  simulate
  steps=$(( steps - 1 ))
done

echo "Part 1: ${one_count}"
