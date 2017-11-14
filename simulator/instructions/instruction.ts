import { Literal } from "../components/basic-types";
import { ExecutionResult } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export enum InstructionCategories {
  Arithmetic,
  Memory,
  Branch,
  IO,
  Misc
}

export abstract class Instruction {
  public name: string;

  abstract get category(): InstructionCategories;

  abstract get duration(): number;

  get halts(): boolean { return false; }

  abstract get requirements(): InstructionInteractions;

  abstract get effects(): InstructionInteractions;

  abstract execute(rf: LikeRegisterFile): ExecutionResult[] | BranchPredictionError;

  static isSuccessfulExecution(executionResults: ExecutionResult[] | BranchPredictionError): executionResults is ExecutionResult[] {
    return Array.isArray(executionResults);
  }

  expectedPC(pc: number): Literal { return pc + 1; }
};

export abstract class ArithmeticInstruction extends Instruction {
  get category() { return InstructionCategories.Arithmetic; }
}

export abstract class MemoryInstruction extends Instruction {
  get category() { return InstructionCategories.Memory; }
}

export abstract class BranchInstruction extends Instruction {
  get category() { return InstructionCategories.Branch; }
}

export abstract class IOInstruction extends Instruction {
  get category() { return InstructionCategories.IO; }
}

export abstract class MiscInstruction extends Instruction {
  get category() { return InstructionCategories.Misc; }
}
