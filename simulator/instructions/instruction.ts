import { PC } from "../components/basic-types";
import { ExecutionResult, RegisterWriter, MemoryWriter, ExternalAction, Halter, BranchPredictionError, RegisterReleaser, MemoryReleaser } from "../components/execution-result";
import { HasRegisters, HasMemory, ReadableRegisterFile } from "../components/register-file";
import { RegisterFileSync } from "../components/register-file-sync";
import { InstructionRequirement, RegisterRequirement, MemoryRequirement } from "../components/instruction-requirements";

export abstract class Instruction {
  readonly name: string;

  abstract getRequirements(sync: RegisterFileSync, rf: ReadableRegisterFile): InstructionRequirement[];

  abstract expectedPC(pc: PC): PC;
};

export abstract class DecodedInstruction<Requirement extends InstructionRequirement, DataSource, Result extends ExecutionResult> extends Instruction {
  abstract get duration(): number;

  abstract getRequirements(sync: RegisterFileSync, rf: ReadableRegisterFile): Requirement[];

  abstract execute(rf: DataSource, pc: PC): Result[];

  get isNonSequential() { return false; }

  expectedPC(pc: PC): PC { return pc + 1; }
}

export abstract class ArithmeticInstruction extends DecodedInstruction<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser> { }

export abstract class MemoryInstruction extends DecodedInstruction<RegisterRequirement | MemoryRequirement, HasRegisters | HasMemory, RegisterWriter | RegisterReleaser | MemoryWriter | MemoryReleaser> { }

export abstract class BranchInstruction extends DecodedInstruction<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser | BranchPredictionError> {
  get isNonSequential() { return true; }

  abstract expectedPC(pc: PC): PC;
}

export abstract class IOInstruction extends DecodedInstruction<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser | ExternalAction>{ }

export abstract class MiscInstruction extends DecodedInstruction<never, never, Halter> {
  abstract get isNonSequential(): boolean;

  abstract expectedPC(pc: PC): PC;
}
