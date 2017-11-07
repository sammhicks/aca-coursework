import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class BranchPredictionError {
  constructor(public writes: RegisterFileWriter[]) { };
};

export abstract class Instruction {
  public name: string;

  abstract get duration(): number;

  get halts(): boolean { return false; }

  abstract get requirements(): InstructionInteractions;

  abstract get effects(): InstructionInteractions;

  abstract execute(rf: RegisterFile): RegisterFileWriter[] | BranchPredictionError;

  static isSuccessfulExecution(executionResults: RegisterFileWriter[] | BranchPredictionError): executionResults is RegisterFileWriter[] {
    return Array.isArray(executionResults);
  }

  expectedPC(pc: number): Literal { return pc + 1; }
};
