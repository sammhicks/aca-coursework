import { PC } from "../components/basic-types";
import { ExecutionResult, RegisterWriter, MemoryWriter, ExternalAction, Halter, BranchPredictionError, RegisterReleaser, MemoryReleaser, TookBranch, Returned, ReturnPredictionError, BranchPredictionSuccess, ReturnPredictionSuccess } from "../components/execution-result";
import { HasRegisters, HasMemory, ReadableRegisterFile } from "../components/register-file";
import { RegisterFileSync } from "../components/register-file-sync";
import { InstructionRequirement, RegisterRequirement, MemoryRequirement } from "../components/instruction-requirements";
import { Prediction } from "../components/prediction";

export abstract class Instruction {
  readonly name: string;

  abstract getRequirements(sync: RegisterFileSync, rf: ReadableRegisterFile): InstructionRequirement[];

  abstract expectedPC(pc: PC, prediction: Prediction): PC;

  abstract get halts(): boolean;
};

export abstract class DecodedInstruction<Requirement extends InstructionRequirement, DataSource, Result extends ExecutionResult> extends Instruction {
  abstract get duration(): number;

  abstract getRequirements(sync: RegisterFileSync, rf: ReadableRegisterFile): Requirement[];

  abstract execute(rf: DataSource, pc: PC, expectedPC: PC): Result[];

  get isNonSequential() { return false; }

  expectedPC(pc: PC, prediction: Prediction): PC { return pc + 1; }
}

export abstract class ArithmeticInstruction extends DecodedInstruction<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser> {
  get halts() { return false; }
}

export abstract class MemoryInstruction extends DecodedInstruction<RegisterRequirement | MemoryRequirement, HasRegisters | HasMemory, RegisterWriter | RegisterReleaser | MemoryWriter | MemoryReleaser> {
  get halts() { return false; }
}

export abstract class BranchInstruction extends DecodedInstruction<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser | TookBranch | BranchPredictionSuccess | BranchPredictionError | Returned | ReturnPredictionSuccess | ReturnPredictionError> {
  get isNonSequential() { return true; }

  get halts() { return false; }

  abstract expectedPC(pc: PC, prediction: Prediction): PC;
}

export abstract class IOInstruction extends DecodedInstruction<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser | ExternalAction>{
  get halts() { return false; }
}

export abstract class MiscInstruction extends DecodedInstruction<never, never, Halter> {
  abstract get isNonSequential(): boolean;

  abstract expectedPC(pc: PC): PC;
}
