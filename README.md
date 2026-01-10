# Advent of Code Solutions Repository

## 2025

- Solved with Javascript (Node.js version v24.11)
- Inputs should be provided as an `input.js` file exporting a single `input` variable
- A template input can be found at `./2025/template.input.js`

## 2024

- Solved with Java (JDK version 23)
- Built with Gradle
- Can be opened with Intellij IDEA

## 2021

- Solved with Zig (0.14.0)
- Should be cross-compatible (untested)
- Can be opened with VSCode using official Zig extension
- Use `zig build -h` and `zig build run` for more info

## 2019

- Solved with Go (1.25.3)
- Can be opened with VSCode using official Go extension
- Use `go run .` once you navigate to a day, e.g. `cd day1`
- Inputs usually expected in `inputs` subfolder, e.g. `day1/inputs/example.txt`
- Exact path needed for an input is usually found in the main function of a day, e.g. the main function in `day1/main.go`

## 2018

- Solved with COBOL (GnuCOBOL 3.2)
- Can be opened with VSCode using SuperBOL extension
- Use `cobc -x DAYX.cob` once you navigate to a day, e.g. `cd day1`
- Exact path needed for an input is usually found in environment division of the corresponding day.
- Some days depend on extra cobol programs, e.g. day 8, use `cobc <PROGNAME>.cob` to compile them as shared libraries
- Almost all solutions are COBOL ANSI-85 compliant or easy to convert to, other than day 8, which uses recursive calls.