package main

type Day interface {
	Run1()
	Run2()
}

func main() {
	days := []Day{
		day1{} }
	for _, d := range days {
		d.Run1();
		d.Run2();
	}
}