package main

import (
	"fmt"
	"math"
	"math/cmplx"
)

func Rotate(h complex128, c int) complex128 {
	r, th := cmplx.Polar(complex128(h))

	th -= float64(c) / 180 * math.Pi

	return cmplx.Rect(r, th)
}

func main() {
	c := 10 + 1i

	fmt.Println(c)

	c = Rotate(c, 90);
	fmt.Println(c)

	c = Rotate(c, -270);
	fmt.Println(c)
}