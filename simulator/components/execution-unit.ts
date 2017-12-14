import { ExecutionResult, PCWriter, RegisterWriter, MemoryWriter, ExternalAction, Halter, BranchPredictionError } from "./execution-result";
import { ReadRequirement, WriteRequirement, ReadsPC, SetsPC, ReadsRegister, SetsRegister, ReadsFromMemory, WritesToMemory } from "./instruction-requirements";
import { ReorderBufferSlot } from "./reorder-buffer";
import { DecodedInstruction } from "../instructions/instruction";
import { Countdown } from "../util/countdown";
import { HasPC, HasRegisters, HasMemory } from "./register-file";

class IdleUnit { }

class ActiveUnit<ReadRequirements extends ReadRequirement, WriteRequirements extends WriteRequirement, DataSource, Results extends ExecutionResult[] | BranchPredictionError> {
  constructor(readonly rf: DataSource, readonly instruction: DecodedInstruction<ReadRequirements, WriteRequirements, DataSource, Results>, readonly reorderBufferSlot: ReorderBufferSlot) { }
}

type ExecutionUnitState<ReadRequirements extends ReadRequirement, WriteRequirements extends WriteRequirement, DataSource, Results extends ExecutionResult[] | BranchPredictionError> = IdleUnit | ActiveUnit<ReadRequirements, WriteRequirements, DataSource, Results>;

export class ExecutionUnit<ReadRequirements extends ReadRequirement, WriteRequirements extends WriteRequirement, DataSource, Results extends ExecutionResult[] | BranchPredictionError> extends Countdown {
  private _state: ExecutionUnitState<ReadRequirements, WriteRequirements, DataSource, Results>;

  constructor(public category: { new(): DecodedInstruction<ReadRequirements, WriteRequirements, DataSource, Results>; }) {
    super();

    this._state = new IdleUnit();
  }

  get isAvailable() {
    return this._state instanceof IdleUnit;
  }

  executeInstruction(rf: DataSource, instruction: DecodedInstruction<ReadRequirements, WriteRequirements, DataSource, Results>, reorderBufferSlot: ReorderBufferSlot) {
    if (this._state instanceof IdleUnit) {
      if (instruction instanceof this.category) {
        this._state = new ActiveUnit<ReadRequirements, WriteRequirements, DataSource, Results>(rf, instruction, reorderBufferSlot);
        reorderBufferSlot.updateInstructionHandler(this);
        this.reset(instruction.duration);
      } else {
        throw Error("Execution Unit can't handle this instruction!");
      }
    } else {
      throw Error("Execution Unit not ready!");
    }
  }

  onCompletion(): void {
    if (this._state instanceof ActiveUnit) {
      const instructionResults = this._state.instruction.execute(this._state.rf);

      if (DecodedInstruction.isSuccessfulExecution(instructionResults)) {
        this._state.reorderBufferSlot.buffer(instructionResults);
      } else {
        //TODO - Handle Branch Failure
      }

      this._state = new IdleUnit();
    } else {
      throw Error("Completed Execution Unit has no instruction!");
    }
  }

  onAborted(): void {
    this._state = new IdleUnit();
  }
}

export type ArithmeticExecutionUnit = ExecutionUnit<ReadsRegister, SetsRegister, HasRegisters, [RegisterWriter]>;
export type MemoryExecutionUnit = ExecutionUnit<ReadsRegister | ReadsFromMemory, SetsRegister | WritesToMemory, HasRegisters | HasMemory, [RegisterWriter | MemoryWriter]>;
export type BranchExecutionUnit = ExecutionUnit<ReadsPC | ReadsRegister, SetsPC | SetsRegister, HasPC | HasRegisters, (PCWriter | RegisterWriter)[] | BranchPredictionError>;
export type IOExecutionUnit = ExecutionUnit<ReadsRegister, SetsRegister, HasRegisters, (RegisterWriter | ExternalAction)[]>;
export type MiscExecutionUnit = ExecutionUnit<never, never, never, [Halter]>;









