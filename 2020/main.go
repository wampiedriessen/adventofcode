package main

import (
	"./day1"
)

type Day interface {
	Run1()
	Run2()
}

func main() {
	days := []Day{
		day1.Day1{} }
	for _, d := range days {
		d.Run1();
		d.Run2();
	}
}