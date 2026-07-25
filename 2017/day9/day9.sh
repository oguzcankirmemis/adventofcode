#!/bin/bash

input=$(sed 's/!.//g' < inputs/input.txt | sed 's/<[^>]*>//g' | sed 's/,//g')
score=0
depth=0

for ((i=0; i<${#input}; i++)); do
  char="${input:${i}:1}"
  if [[ "${char}" = "{" ]]; then
    depth=$(( depth + 1 ))
  fi
  if [[ "${char}" = "}" ]]; then
    score=$(( score + depth ))
    depth=$(( depth - 1 ))
  fi
done

echo "Part 1: ${score}"


input=$(sed 's/!.//g' < inputs/input.txt)
original_length=${#input}
input=$(sed 's/<[^>]*>/--/g' <<< "${input}")
modified_length=${#input}
deleted=$(( original_length - modified_length ))

echo "Part 2: ${deleted}"
