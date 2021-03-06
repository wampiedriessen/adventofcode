package helpers

import (
	"bufio"
	"log"
	"os"
	"reflect"
	"strconv"
	"strings"
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

func CloneValue(source interface{}, destin interface{}) {
    x := reflect.ValueOf(source)
    if x.Kind() == reflect.Ptr {
        starX := x.Elem()
        y := reflect.New(starX.Type())
        starY := y.Elem()
        starY.Set(starX)
        reflect.ValueOf(destin).Elem().Set(y.Elem())
    } else {
        destin = x.Interface()
    }
}

func SplitStringToInts(Str string, Split string) []int {
    a := strings.Split(Str, Split)
    b := make([]int, len(a))
    for i, v := range a {
        b[i], _ = strconv.Atoi(v)
    }
    return b
}