package common

import (
	"os"
)

func Read(inputPath string) string {
	dat, err := os.ReadFile(inputPath)
	if err != nil {
		panic(err)
	}
	return string(dat)
}
