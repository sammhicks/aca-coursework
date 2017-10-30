import { Instruction } from "../instructions/instruction"
import { RegisterFile } from "./register-file"

export class InstructionQueue {
  private _instructions: Instruction[]
  private _size: number;

  constructor(size: number) {
    this._instructions = [];
    this._size = size;
  }

  load(rf: RegisterFile, instructions: Instruction[]) {
    var pc = rf.pc;
    while (this._instructions.length < this._size) {
      var instruction = instructions[pc];

      if (instruction.halts) {
        break;
      }

      this._instructions.push(instruction);
      pc = instruction.expectedPC(pc);
    }
  }
}
