#!/bin/bash

instructions=()
regs=()
pc=0
mul_count=0

while read instr; do
  instructions[${#instructions[@]}]=${instr}
done < inputs/input.txt

for ((i=0; ${i}<8; i++)); do
  regs[${i}]=0
done

function to_val () {
  local op=${1}
  local val=$(printf "%d" "'${op}")
  if (( ${val} >= 97 )); then
    val=$(( val - 97 ))
    printf "%s" "${regs[val]}"
  else
    printf "%s" "${op}"
  fi
}

function simulate () {
  local instr=$(cut -d " " -f 1 <<< "${instructions[pc]}")
  local op1=$(cut -d " " -f 2 <<< "${instructions[pc]}")
  local op2=$(cut -d " " -f 3 <<< "${instructions[pc]}")
  local val1=$(to_val "${op1}")
  op1=$(printf "%d" "'${op1}")
  op1=$(( op1 - 97 ))
  op2=$(to_val "${op2}")
  if [[ "${instr}" = "set" ]]; then
    regs[${op1}]=${op2}
    pc=$(( pc + 1 ))
  elif [[ "${instr}" = "sub" ]]; then
    regs[${op1}]=$(( regs[op1] - op2 ))
    pc=$(( pc + 1 ))
  elif [[ "${instr}" = "mul" ]]; then
    regs[${op1}]=$(( regs[op1] * op2 ))
    pc=$(( pc + 1 ))
    mul_count=$(( mul_count + 1 ))
  elif [[ "${instr}" = "jnz" ]]; then
    if (( ${val1} != 0 )); then
      pc=$(( pc + op2 ))
    else
      pc=$(( pc + 1 ))
    fi
  else
    echo "Illegal Instruction"
    return 1
  fi
}

while (( ${pc} >= 0 )) && (( ${pc} < ${#instructions[@]} )); do
  simulate
done

echo "Part 1: ${mul_count}"

for ((i=0; ${i}<8; i++)); do
  regs[${i}]=0
done
regs[0]=1

function is_non_prime () {
  local num=${1}
  local till=$(bc <<< "sqrt(${num})")
  local i=""
  for ((i=2; ${i} <= ${till}; i++)); do
    if (( ${num} % ${i} == 0 )); then
      return 0
    fi
  done
  return 1
}

function program_optimized () {
  regs[1]=106500
  local i=""
  for ((i=0; ${i}<=1000; i++)) do
    if is_non_prime "${regs[1]}"; then
      regs[7]=$(( regs[7] + 1 ))
    fi
    regs[1]=$(( regs[1] + 17 ))
  done
}

program_optimized

echo "Part 2: ${regs[7]}"
