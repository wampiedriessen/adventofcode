package helpers

import (
	"bufio"
	"log"
	"os"
	"strconv"
)

func GetListOfStrings(filename string) []string {
	file, err := os.Open(filename)
    if err != nil {
        log.Fatal(err)
    }
    defer file.Close()

	scanner := bufio.NewScanner(file)
	var result []string
    for scanner.Scan() {
        result = append(result, scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        log.Fatal(err)
	}
	return result
}

func GetListOfInts(filename string) []int {
	var result []int
	for _, v := range GetListOfStrings(filename) {
		val, _ := strconv.Atoi(v)
		result = append(result, val)
	}
	return result
}