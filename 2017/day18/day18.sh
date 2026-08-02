#!/bin/bash

program=()
last_played=""
last_recovered=""
first_recovered=""

regs0=()
queue0=()
pc0=0
hang0="false"

regs1=()
queue1=()
pc1=0
hang1="false"
snd1_count=0

while read instr; do
  program[${#program[@]}]="${instr}"
done < inputs/input.txt

for ((i=0; ${i}<26; i++)); do
  regs0[${i}]=0
done

function reg_to_idx () {
  local reg=${1}
  local char=$(printf "%d" "'${reg}")
  reg=$(( char - 97 ))
  printf "%s" "${reg}"
}

function arg_to_val () {
  local reg=${1}
  local computer=${2}
  local char=$(printf "%d" "'${reg}")
  if (( ${char} < 97 )); then
    printf "%s" "${reg}"
  else
    reg=$(( char - 97 ))
    if (( ${computer} == 0 )); then
      printf "%s" "${regs0[${reg}]}"
    else
      printf "%s" "${regs1[${reg}]}"
    fi
  fi
}

function snd_instr () {
  local computer=${2}
  local freq=$(arg_to_val "${1}" "${computer}")
  last_played=${freq}
  if (( ${computer} == 0 )); then
    queue1[${#queue1[@]}]=${freq}
  else
    queue0[${#queue0[@]}]=${freq}
    snd1_count=$(( snd1_count + 1 ))
  fi
}

function set_instr () {
  local computer=${3}
  local reg=$(reg_to_idx "${1}")
  local val=$(arg_to_val "${2}" "${computer}")
  if (( ${computer} == 0 )); then
    regs0[${reg}]=${val}
  else
    regs1[${reg}]=${val}
  fi
}

function add_instr () {
  local computer=${3}
  local reg=$(reg_to_idx "${1}")
  local val=$(arg_to_val "${2}" "${computer}")
  if (( ${computer} == 0 )); then
    regs0[${reg}]=$(( regs0[reg] + val ))
  else
    regs1[${reg}]=$(( regs1[reg] + val ))
  fi
}

function mul_instr () {
  local computer=${3}
  local reg=$(reg_to_idx "${1}")
  local val=$(arg_to_val "${2}" "${computer}")
  if (( ${computer} == 0 )); then
    regs0[${reg}]=$(( regs0[reg] * val ))
  else
    regs1[${reg}]=$(( regs1[reg] * val ))
  fi
}

function mod_instr () {
  local computer=${3}
  local reg=$(reg_to_idx "${1}")
  local val=$(arg_to_val "${2}" "${computer}")
  if (( ${computer} == 0 )); then
    regs0[${reg}]=$(( regs0[reg] % val ))
  else
    regs1[${reg}]=$(( regs1[reg] % val ))
  fi
}

function rcv_instr () {
  local computer=${2}
  local val=$(arg_to_val "${1}" "${computer}")
  if (( ${val} != 0 )); then
    last_recovered=${last_played}
  fi
  if [[ "${first_recovered}" = "" ]]; then
    if (( ${computer} == 0 )); then
      pc0=$(( pc0 + 1 ))
    else
      pc1=$(( pc1 + 1 ))
    fi
  else
    local reg=$(reg_to_idx "${1}")
    if (( ${computer} == 0 )); then
      if (( ${#queue0[@]} == 0 )); then
        hang0="true"
      else
        val=${queue0[0]}
        queue0=("${queue0[@]:1}")
        regs0[${reg}]=${val}
        pc0=$(( pc0 + 1 ))
        hang0="false"
      fi
    else
      if (( ${#queue1[@]} == 0 )); then
        hang1="true"
      else
        val=${queue1[0]}
        queue1=("${queue1[@]:1}")
        regs1[${reg}]=${val}
        pc1=$(( pc1 + 1 ))
        hang1="false"
      fi
    fi
  fi
}

function jgz_instr () {
  local computer=${3}
  local val1=$(arg_to_val "${1}" "${computer}")
  local val2=$(arg_to_val "${2}" "${computer}")
  if (( ${computer} == 0 )); then
    if (( ${val1} > 0 )); then
      pc0=$(( pc0 + val2 ))
    else
      pc0=$(( pc0 + 1 ))
    fi
  else
    if (( ${val1} > 0 )); then
      pc1=$(( pc1 + val2 ))
    else
      pc1=$(( pc1 + 1 ))
    fi
  fi
}

function deadlock () {
  if (( ${pc0} < 0 || ${pc0} >= ${#program[@]} )); then
    if (( ${pc1} < 0 || ${pc1} >= ${#program[@]} )); then
      return 0
    fi
    if [[ "${hang1}" = "true" ]]; then
      return 0
    fi
    return 1
  elif (( ${pc1} < 0 || ${pc1} >= ${#program[@]} )); then
    if [[ "${hang0}" = "true" ]]; then
      return 0
    fi
    return 1
  elif [[ "${hang0}" = "true" ]] && [[ "${hang1}" = "true" ]]; then
    return 0
  else
    return 1
  fi
}

function exec_single () {
  local computer=${1}
  local pc=""
  if (( ${computer} == 0 )); then
    pc=${pc0}
    if (( ${pc0} < 0 )) || (( ${pc0} >= ${#program[@]} )); then
      return 1
    fi
  else
    pc=${pc1}
    if (( ${pc1} < 0 )) || (( ${pc1} >= ${#program[@]} )); then
      return 1
    fi
  fi
  local instr=$(cut -d " " -f 1 <<< "${program[${pc}]}")
  local arg1=$(cut -d " " -f 2 <<< "${program[${pc}]}")
  local arg2=$(cut -d " " -f 3 <<< "${program[${pc}]}")
  if (( ${computer} == 0 )); then
    pc0=$(( pc0 + 1 ))
  else
    pc1=$(( pc1 + 1 ))
  fi
  if [[ "${instr}" = "snd" ]]; then
    snd_instr "${arg1}" "${computer}"
  elif [[ "${instr}" = "set" ]]; then
    set_instr "${arg1}" "${arg2}" "${computer}"
  elif [[ "${instr}" = "add" ]]; then
    add_instr "${arg1}" "${arg2}" "${computer}"
  elif [[ "${instr}" = "mul" ]]; then
    mul_instr "${arg1}" "${arg2}" "${computer}"
  elif [[ "${instr}" = "mod" ]]; then
    mod_instr "${arg1}" "${arg2}" "${computer}"
  elif [[ "${instr}" = "rcv" ]]; then
    if (( ${computer} == 0 )); then
      pc0=$(( pc0 - 1 ))
    else
      pc1=$(( pc1 - 1 ))
    fi
    rcv_instr "${arg1}" "${computer}"
    if [[ "${first_recovered}" = "" ]]; then
      first_recovered="${last_recovered}"
    fi
  elif [[ "${instr}" = "jgz" ]]; then
    if (( ${computer} == 0 )); then
      pc0=$(( pc0 - 1 ))
    else
      pc1=$(( pc1 - 1 ))
    fi
    jgz_instr "${arg1}" "${arg2}" "${computer}"
  else
    echo "Invalid Instruction"
    return 1
  fi
}

while [[ "${first_recovered}" = "" ]]; do
  exec_single 0
done

echo "Part 1: ${first_recovered}"

for ((i=0; ${i}<26; i++)); do
  regs0[${i}]=0
  regs1[${i}]=0
done
pid=$(printf "%d" "'p")
pid=$(( pid - 97 ))
regs0[${pid}]=0
regs1[${pid}]=1
pc0=0
pc1=0
hang0="false"
hang1="false"
queue0=()
queue1=()
snd1_count=0

while ! deadlock; do
  exec_single 0
  exec_single 1
done

echo "Part 2: ${snd1_count}"
