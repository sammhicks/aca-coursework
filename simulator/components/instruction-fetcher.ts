import { PC } from "./basic-types";
import { Instruction } from "../instructions/instruction";
import { Prediction } from "./prediction";

export class InstructionFetcher {
  private _pc: PC;
  private _instructions: Instruction[];
  private _prediction: Prediction;

  constructor(instructions: Instruction[], prediction: Prediction) {
    this._pc = 0;
    this._instructions = instructions;
    this._prediction = prediction;
  }

  load(): [PC, Instruction] {
    const pc = this._pc;
    const instruction = this._instructions[pc];
    this._pc = instruction.expectedPC(pc, this._prediction);

    return [pc, instruction];
  }

  reset(pc: PC) { this._pc = pc; }
}
