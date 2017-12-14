import { PC } from "./basic-types";
import { HandlesBranchPredictionError } from "./register-file";
import { Instruction } from "../instructions/instruction"

export class InstructionFetcher implements HandlesBranchPredictionError {
  private _pc: PC;
  private _instructions: Instruction[]

  constructor(instructions: Instruction[]) {
    this._pc = 0;
    this._instructions = instructions;
  }

  load() {
    const instruction = this._instructions[this._pc];
    this._pc = instruction.expectedPC(this._pc);

    return instruction;
  }

  handleBranchPredictionError(pc: PC) { this._pc = pc; }
}
