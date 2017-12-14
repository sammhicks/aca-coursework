import { PC } from "../components/basic-types";
import { ExecutionResult, BranchPredictionError, PCWriter, RegisterWriter, MemoryWriter, ExternalAction, Halter } from "../components/execution-result";
import { HasRegisterFileComponents, HasPC, HasRegisters, HasMemory, RegisterFile } from "../components/register-file";
import { RegisterFileSync } from "../components/register-file-sync";
import { ReadRequirement, WriteRequirement, ReadsPC, SetsPC, ReadsRegister, SetsRegister, ReadsFromMemory, WritesToMemory, EmptyReadRequirement, EmptyWriteRequirement } from "../components/instruction-requirements";

export abstract class Instruction {
  readonly name: string;
};

export abstract class DecodedInstruction<ReadRequirements extends ReadRequirement, WriteRequirements extends WriteRequirement, DataSource, Results extends ExecutionResult[] | BranchPredictionError> {
  abstract get duration(): number;

  abstract getReadRequirements(sync: RegisterFileSync, rf: RegisterFile): ReadRequirements[];

  abstract getWriteRequirements(sync: RegisterFileSync, rf: RegisterFile): WriteRequirements[];

  abstract execute(rf: DataSource): Results;

  static isSuccessfulExecution(executionResults: ExecutionResult[] | BranchPredictionError): executionResults is ExecutionResult[] {
    return Array.isArray(executionResults);
  }

  expectedPC(pc: PC): PC { return pc + 1; }
}

export abstract class ArithmeticInstruction extends DecodedInstruction<ReadsRegister, SetsRegister, HasRegisters, [RegisterWriter]> { }

export abstract class MemoryInstruction extends DecodedInstruction<ReadsRegister | ReadsFromMemory, SetsRegister | WritesToMemory, HasRegisters | HasMemory, [RegisterWriter | MemoryWriter]> { }

export abstract class BranchInstruction extends DecodedInstruction<ReadsPC | ReadsRegister, SetsPC | SetsRegister, HasPC | HasRegisters, (PCWriter | RegisterWriter)[] | BranchPredictionError> { }

export abstract class IOInstruction extends DecodedInstruction<ReadsRegister, SetsRegister, HasRegisters, (RegisterWriter | ExternalAction)[]>{ }

export abstract class MiscInstruction extends DecodedInstruction<never, never, HasRegisterFileComponents, [Halter]> { }
