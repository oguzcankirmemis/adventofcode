#!/bin/bash

valid=0

while read line; do
  sorted=$(echo ${line} | tr " " "\n" | sort | tr "\n" " ")
  prev_word=""
  is_valid="true"
  for word in ${sorted}; do
    if [[ "${word}" == "${prev_word}" ]]; then
      is_valid="false"
      break
    fi
    prev_word="${word}"
  done
  if [[ "${is_valid}" == "true" ]]; then
    valid=$(( valid + 1 ))
  fi
done < inputs/input.txt

echo "Part 1: ${valid}"

valid=0

while read line; do
  sorted_array=()
  for word in ${line}; do
    sorted_word=$(echo "${word}" | fold -w1 | sort | tr -d "\n")
    sorted_array[${#sorted_array[@]}]="${sorted_word}"
  done
  sorted=$(printf "%s\n" "${sorted_array[@]}" | sort | tr "\n" " ")
  prev_word=""
  is_valid="true"
  for word in ${sorted}; do
    if [[ "${word}" == "${prev_word}" ]]; then
      is_valid="false"
      break
    fi
    prev_word="${word}"
  done
  if [[ "${is_valid}" == "true" ]]; then
    valid=$(( valid + 1 ))
  fi
done < inputs/input.txt

echo "Part 2: ${valid}"
