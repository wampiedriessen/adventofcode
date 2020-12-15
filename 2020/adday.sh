if [ -z "$1" ]; then
    echo "No day number supplied"
    exit
fi

mkdir -p "day$1"

cat > "day$1/day$1.go" << EOF
package DAYNAME

import (
	"fmt"
	"helpers"
)

type Day struct {}

var filename = "/home/wampie/git/adventofcode/2020/inputs/DAYNAME"

func (d Day) Run1() string {
	var parsedinput = helpers.GetListOfInts(filename)
	return fmt.Sprint(part1(parsedinput))
}

func (d Day) Run2() string {
	var parsedinput = helpers.GetListOfInts(filename)
	return fmt.Sprint(part2(parsedinput))
}

func part1(arr []int) int {
	return 0;
}

func part2(arr []int) int {
	return 0;
}
EOF

cat > "day$1/day$1_test.go" << EOF
package DAYNAME

import "testing"

func Test1(t *testing.T) {
	var input = []int{1721, 979, 366, 299, 675, 1456};

	var output = part1(input);

	if output != 514579 {
		t.Errorf("Not correct!: %v", output);
	}
}

func Test2(t *testing.T) {
	var input = []int{1721, 979, 366, 299, 675, 1456};

	var output = part2(input);

	if output != 241861950 {
		t.Errorf("Not correct!: %v", output);
	}
}

func TestPart1(t *testing.T) {
	d := Day{}
	val := d.Run1()
	t.Log(val)
	if val != "0" {
		t.Error("Regression on p1")
	}
}

func TestPart2(t *testing.T) {
	d := Day{}
	val := d.Run2 ()
	t.Log(val)
	if val != "0" {
		t.Error("Regression on p2")
	}
}
EOF

cat > "day$1/go.mod" << EOF
module DAYNAME

go 1.13

require helpers v0.0.0
replace helpers v0.0.0 => ../helpers

EOF

sed -i "s/DAYNAME/day$1/g" "day$1/go.mod"
sed -i "s/DAYNAME/day$1/g" "day$1/day$1.go"
sed -i "s/DAYNAME/day$1/g" "day$1/day$1_test.go"
if ! grep -q "day$1" main.go; then
    sed -i "20 a \		Conf {\"Day $1\", day$1.Day{} }," main.go
fi
if ! grep -q "day$1" go.mod; then
    echo "require day$1 v0.0.0" >> go.mod
    echo "replace day$1 v0.0.0 => ./day$1" >> go.mod
fi