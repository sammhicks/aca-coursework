import { Literal } from "../components/basic-types";
import { ExecutionResult, BranchPredictionError, PCWriter, RegisterWriter, MemoryWriter, ExternalAction, Halter } from "../components/execution-result";
import { HasRegisterFileComponents, HasPC, HasRegisters, HasMemory } from "../components/register-file";
import { InstructionInteractions, ArithmeticInteractions, MemoryInteractions, BranchInteractions, IOInteractions, MiscInteractions } from "../components/instruction-interactions";

export abstract class Instruction {
  readonly name: string;
};

export abstract class DecodedInstruction<Requirements extends InstructionInteractions, Effects extends InstructionInteractions, DataSource, Results extends ExecutionResult[] | BranchPredictionError> {
  abstract get duration(): number;

  get halts(): boolean { return false; }

  abstract get requirements(): Requirements;

  abstract get effects(): Effects;

  abstract execute(rf: DataSource): Results;

  static isSuccessfulExecution(executionResults: ExecutionResult[] | BranchPredictionError): executionResults is ExecutionResult[] {
    return Array.isArray(executionResults);
  }

  expectedPC(pc: number): Literal { return pc + 1; }
}

export abstract class ArithmeticInstruction extends DecodedInstruction<ArithmeticInteractions, ArithmeticInteractions, HasRegisters, [RegisterWriter]> { }

export abstract class MemoryInstruction extends DecodedInstruction<MemoryInteractions, MemoryInteractions, HasRegisters | HasMemory, [RegisterWriter | MemoryWriter]> { }

export abstract class BranchInstruction extends DecodedInstruction<BranchInteractions, BranchInteractions, HasPC | HasRegisters, (PCWriter | RegisterWriter)[] | BranchPredictionError> { }

export abstract class IOInstruction extends DecodedInstruction<IOInteractions, IOInteractions, HasRegisters, (RegisterWriter | ExternalAction)[]>{ }

export abstract class MiscInstruction extends DecodedInstruction<MiscInteractions, MiscInteractions, HasRegisterFileComponents, [Halter]> { }
