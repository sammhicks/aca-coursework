import { PC } from "./basic-types";
import { Instruction } from "../instructions/instruction";

export class InstructionFetcher {
  private _pc: PC;
  private _instructions: Instruction[]

  constructor(instructions: Instruction[]) {
    this._pc = 0;
    this._instructions = instructions;
  }

  load(): [PC, Instruction] {
    const pc = this._pc;
    const instruction = this._instructions[pc];
    this._pc = instruction.expectedPC(pc);

    return [pc, instruction];
  }

  reset(pc: PC) { this._pc = pc; }
}
