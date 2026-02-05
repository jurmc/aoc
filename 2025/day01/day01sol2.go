package main

import (
	"fmt"
	"os"
	"bufio"
)

func main() {
	fmt.Println("Solution (test): ", solve("test_input.txt"))
	fmt.Println("Solution: ", solve("input.txt"))
}

func solve(filename string) int {
	dial := 50
	acc := 0
	f, err := os.Open(filename)
	if err != nil {
		panic("Cannot open file")
	}
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		var dir rune
		var val int
		_, err = fmt.Sscanf(line, "%c%d", &dir, &val)
		if err != nil {
			panic("Cannot scan line")
		}
		if dir == 'L' {
			val = -val
		}
		dial = dial + val
		if dial < 0 {
			dial = dial % 100
			dial = dial + 100
		}
		if dial > 99 {
			dial = dial % 100
		}
		if dial < 0 {
			panic("dupa zbita")
		}
		if dial == 0 {
			acc++
		}
	}

	return acc
}
