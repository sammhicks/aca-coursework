import { Literal } from "../components/basic-types";
import { ExecutionResult, BranchPredictionError, PCWriter, RegisterWriter, MemoryWriter, ExternalAction, Halter } from "../components/execution-result";
import { HasRegisterFileComponents, HasPC, HasRegisters, HasMemory, PerformsExternalInteractions, Halts } from "../components/register-file";
import { InstructionInteractions, ArithmeticInteractions, MemoryInteractions, BranchInteractions, IOInteractions, MiscInteractions } from "../components/instruction-interactions";

export abstract class Instruction {
  public name: string;

  abstract get duration(): number;

  get halts(): boolean { return false; }

  abstract get requirements(): InstructionInteractions;

  abstract get effects(): InstructionInteractions;

  abstract execute(rf: HasRegisterFileComponents): ExecutionResult[] | BranchPredictionError;

  static isSuccessfulExecution(executionResults: ExecutionResult[] | BranchPredictionError): executionResults is ExecutionResult[] {
    return Array.isArray(executionResults);
  }

  expectedPC(pc: number): Literal { return pc + 1; }
};

export abstract class ArithmeticInstruction extends Instruction {
  abstract get requirements(): ArithmeticInteractions;
  abstract get effects(): ArithmeticInteractions;
  abstract execute(rf: HasRegisters): [RegisterWriter];
}

export abstract class MemoryInstruction extends Instruction {
  abstract get requirements(): MemoryInteractions;
  abstract get effects(): MemoryInteractions;
  abstract execute(rf: HasRegisters & HasMemory): [RegisterWriter | MemoryWriter];
}

export abstract class BranchInstruction extends Instruction {
  abstract get requirements(): BranchInteractions;
  abstract get effects(): BranchInteractions;
  abstract execute(rf: HasPC & HasRegisters): (PCWriter | RegisterWriter)[] | BranchPredictionError;
}

export abstract class IOInstruction extends Instruction {
  abstract get requirements(): IOInteractions;
  abstract get effects(): IOInteractions;
  abstract execute(rf: HasRegisters): (RegisterWriter | ExternalAction)[];
}

export abstract class MiscInstruction extends Instruction {
  abstract get requirements(): MiscInteractions;
  abstract get effects(): MiscInteractions;
  abstract execute(rf: Halts): [Halter];
}
