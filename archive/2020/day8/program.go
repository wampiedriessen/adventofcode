package day8

type Instruction struct {
	op string
	val int
}

type Program struct {
	acc int
	pc int
	instructions []Instruction
}

func (p *Program) Step() {
	inst := p.instructions[p.pc]
	switch inst.op {
	case "jmp":
		p.pc += inst.val
		return
	case "acc":
		p.acc += inst.val
	case "nop":
	default:
	}
	p.pc++
}

func (p *Program) ChangeCurrentOp(op string) {
	p.instructions[p.pc].op = op
}

func (p *Program) GetCurrentOp() string {
	return p.instructions[p.pc].op
}

func (p *Program) Run() {
	for !p.IsFinished() {
		p.Step()
	}
}

func (p *Program) IsFinished() bool {
	return p.pc >= len(p.instructions)
}