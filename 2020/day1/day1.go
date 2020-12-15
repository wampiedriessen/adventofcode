package day1

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	
)

type Day1 struct {}

func (d Day1) Run1() {
	var parsedinput = parse()
	fmt.Printf("%v\n", part1(parsedinput, 2020))
}

func (d Day1) Run2() {
	var parsedinput = parse()
	fmt.Printf("%v\n", part2(parsedinput))
}

func parse() []int {
    file, err := os.Open("/home/wampie/git/adventofcode/2020/inputs/day1")
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

	scanner := bufio.NewScanner(file)
	var result []int
    for scanner.Scan() {
		val, _ := strconv.Atoi(scanner.Text())
        result = append(result, val)
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
	}
	return result
}


func part1(arr []int, target int) int {
	var i, j int;
	for i = 0; i < len(arr); i++ {
		for j = (i+1); j < len(arr); j++ {
			if arr[j]+arr[i] == target {
				return arr[j]*arr[i];
			}
		}
	}
	return 0;
}

func part2(arr []int) int {
	var i int;
	for i = 0; i < len(arr)-1; i++ {
		val := part1(arr[i+1:], 2020 - arr[i])
		if val != 0 {
			return val * arr[i]
		}
	}
	return 0;
}
