#!/bin/bash

checksum1=0
checksum2=0

while read line; do
  sorted=$(echo "${line}" | tr "[:blank:]" "\n" | sort -g)
  max=$(echo "${sorted}" | tail -1)
  min=$(echo "${sorted}" | head -1)
  checksum1=$(( checksum1 + max - min ))
done < inputs/input.txt

echo "Part 1: ${checksum1}"

while read line; do
  for num1 in ${line}; do
    for num2 in ${line}; do
      if [[ "${num1}" != "${num2}" ]] && (( ${num1} % ${num2} == 0 || ${num2} % ${num1} == 0 )); then
        checksum2=$(( checksum2 + (num1 / num2) + (num2 / num1) ))
        break 2
      fi
    done
  done
done < inputs/input.txt

echo "Part 2: ${checksum2}"
