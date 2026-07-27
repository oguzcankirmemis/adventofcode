#!/bin/bash

IFS="," read -a input < inputs/input.txt

n=0
ne=0
nw=0
s=0
se=0
sw=0

for dir in "${input[@]}"; do
  if [[ "${dir}" = "n" ]]; then
    n=$(( n + 1 ))
  fi
  if [[ "${dir}" = "ne" ]]; then
    ne=$(( ne + 1 ))
  fi
  if [[ "${dir}" = "nw" ]]; then
    nw=$(( nw + 1 ))
  fi
  if [[ "${dir}" = "s" ]]; then
    s=$(( s + 1 ))
  fi
  if [[ "${dir}" = "se" ]]; then
    se=$(( se + 1 ))
  fi
  if [[ "${dir}" = "sw" ]]; then
    sw=$(( sw + 1 ))
  fi
done

function reduce () {
  local prev=$(( n + ne + nw + s + se + sw ))
  local reduced="false"
  if (( ${n} > ${s} )); then
    n=$(( n - s ))
    s=0
  else
    s=$(( s - n ))
    n=0
  fi
  if (( ${nw} > ${se} )); then
    nw=$(( nw - se ))
    se=0
  else
    se=$(( se - nw ))
    nw=0
  fi
  if (( ${ne} > ${sw} )); then
    ne=$(( ne - sw ))
    sw=0
  else
    sw=$(( sw - ne ))
    ne=0
  fi
  local next=$(( n + ne + nw + s + se + sw ))
  if (( ${next} < ${prev} )); then
    reduced="true"
  fi
  printf "%s" "${reduced}"
}

function shorten () {
  local prev=$(( n + ne + nw + s + se + sw ))
  local shortened="false"
  if (( ${n} > ${se} )); then
    n=$(( n - se ))
    ne=$(( ne + se ))
    se=0
  else
    se=$(( se - n ))
    ne=$(( ne + n ))
    n=0
  fi
  if (( ${n} > ${sw} )); then
    n=$(( n - sw ))
    nw=$(( nw + sw ))
    sw=0
  else
    sw=$(( sw - n ))
    nw=$(( nw + n ))
    n=0
  fi
  if (( ${s} > ${ne} )); then
    s=$(( s - ne ))
    se=$(( se + ne ))
    ne=0
  else
    ne=$(( ne - s ))
    se=$(( se + s ))
    s=0
  fi
  if (( ${s} > ${nw} )); then
    s=$(( s - nw ))
    sw=$(( sw + nw ))
    nw=0
  else
    nw=$(( nw - s ))
    sw=$(( sw + s ))
    s=0
  fi
  if (( ${ne} > ${nw} )); then
    ne=$(( ne - nw ))
    n=$(( n + nw ))
    nw=0
  else
    nw=$(( nw - ne ))
    n=$(( n + ne ))
    ne=0
  fi
  if (( ${se} > ${sw} )); then
    se=$(( se - sw ))
    s=$(( s + sw ))
    sw=0
  else
    sw=$(( sw - se ))
    s=$(( s + se ))
    se=0
  fi
  local next=$(( n + ne + nw + s + se + sw ))
  if (( ${next} < ${prev} )); then
    shortened="true"
  fi
  printf "%s" "${shortened}"
}

while [[ "$(reduce)" = "true" ]] || [[ "$(shorten)" = "true" ]]; do
  reduce > /dev/null
  shorten > /dev/null
done

result=$(( n + ne + nw + s + se + sw ))

echo "Part 1: ${result}"

n=0
ne=0
nw=0
s=0
se=0
sw=0

max=0

for dir in "${input[@]}"; do
  if [[ "${dir}" = "n" ]]; then
    n=$(( n + 1 ))
  fi
  if [[ "${dir}" = "ne" ]]; then
    ne=$(( ne + 1 ))
  fi
  if [[ "${dir}" = "nw" ]]; then
    nw=$(( nw + 1 ))
  fi
  if [[ "${dir}" = "s" ]]; then
    s=$(( s + 1 ))
  fi
  if [[ "${dir}" = "se" ]]; then
    se=$(( se + 1 ))
  fi
  if [[ "${dir}" = "sw" ]]; then
    sw=$(( sw + 1 ))
  fi
  while [[ "$(reduce)" = "true" ]] || [[ "$(shorten)" = "true" ]]; do
    reduce > /dev/null
    shorten > /dev/null
  done
  current=$(( n + ne + nw + s + se + sw ))
  if (( ${current} > ${max} )); then
    max=${current}
  fi
done

echo "Part 2: ${max}"
