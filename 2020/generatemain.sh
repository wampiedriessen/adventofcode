cat > "main.go" << EOF
package main

import (
    "fmt"
EOF

for i in {1..26}; do
    [ -d "day$i" ] && echo "  \"day$i\"" >> "main.go"
done

cat >> "main.go" << EOF
)

type Day interface {
	Run1() string
	Run2() string
}

type Conf struct {
	Name string
	Day Day
}

func main() {
	days := []Conf{
EOF

for i in {1..26}; do
    [ -d "day$i" ] && echo "      Conf {\"Day $i\", day$i.Day{} }," >> "main.go"
done

cat >> "main.go" << EOF
	}
	for _, d := range days {
		fmt.Println(d.Name)
		fmt.Println(d.Day.Run1());
		fmt.Println(d.Day.Run2());
	}
}
EOF

cat > "go.mod" << EOF
module aoc/2020

go 1.13

require helpers v0.0.0

replace helpers v0.0.0 => ./helpers
EOF

for i in {1..26}; do
    [ -d "day$i" ] && echo "require day$i v0.0.0" >> "go.mod"
    [ -d "day$i" ] && echo "replace day$i v0.0.0 => ./day$i" >> "go.mod"
done

