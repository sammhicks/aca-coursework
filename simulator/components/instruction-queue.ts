import { Instruction } from "../instructions"
import { RegisterFile } from "./register-file"

export class InstructionQueue {
  private instructions: Instruction[]

  constructor(private size: number) {
    this.instructions = [];
  }

  load(rf: RegisterFile, instructions: Instruction[]) {
    var pc = rf.pc;
    while (this.instructions.length < this.size) {
      var instruction = instructions[pc];
      this.instructions.push(instruction);
      pc = instruction.expectedPC(pc);
    }
  }
}
