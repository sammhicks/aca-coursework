import { PC } from "../components/basic-types";
import { ExecutionResult, RegisterWriter, MemoryWriter, ExternalAction, Halter, BranchPredictionError } from "../components/execution-result";
import { HasRegisters, HasMemory, RegisterFile } from "../components/register-file";
import { RegisterFileSync } from "../components/register-file-sync";
import { ReadRequirement, WriteRequirement, ReadsRegister, SetsRegister, ReadsFromMemory, WritesToMemory } from "../components/instruction-requirements";

export abstract class Instruction {
  readonly name: string;

  abstract getReadRequirements(sync: RegisterFileSync, rf: RegisterFile): ReadRequirement[];
  abstract getWriteRequirements(sync: RegisterFileSync, rf: RegisterFile): WriteRequirement[];

  abstract expectedPC(pc: PC): PC;
};

export abstract class DecodedInstruction<ReadRequirements extends ReadRequirement, WriteRequirements extends WriteRequirement, DataSource, Result extends ExecutionResult> extends Instruction {
  abstract get duration(): number;

  abstract getReadRequirements(sync: RegisterFileSync, rf: RegisterFile): ReadRequirements[];

  abstract getWriteRequirements(sync: RegisterFileSync, rf: RegisterFile): WriteRequirements[];

  abstract execute(rf: DataSource, pc: PC): Result[];

  get isNonSequential() { return false; }

  expectedPC(pc: PC): PC { return pc + 1; }
}

export abstract class ArithmeticInstruction extends DecodedInstruction<ReadsRegister, SetsRegister, HasRegisters, RegisterWriter> { }

export abstract class MemoryInstruction extends DecodedInstruction<ReadsRegister | ReadsFromMemory, SetsRegister | WritesToMemory, HasRegisters | HasMemory, RegisterWriter | MemoryWriter> { }

export abstract class BranchInstruction extends DecodedInstruction<ReadsRegister, SetsRegister, HasRegisters, RegisterWriter | BranchPredictionError> {
  get isNonSequential() { return true; }

  abstract expectedPC(pc: PC): PC;
}

export abstract class IOInstruction extends DecodedInstruction<ReadsRegister, SetsRegister, HasRegisters, RegisterWriter | ExternalAction>{ }

export abstract class MiscInstruction extends DecodedInstruction<never, never, never, Halter> {
  abstract get isNonSequential(): boolean;

  abstract expectedPC(pc: PC): PC;
}
