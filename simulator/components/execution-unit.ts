import { PC } from "./basic-types";
import { ExecutionResult, RegisterWriter, MemoryWriter, ExternalAction, Halter, BranchPredictionError, ExecutionResultsHandler } from "./execution-result";
import { ReadRequirement, WriteRequirement, ReadsRegister, SetsRegister, ReadsFromMemory, WritesToMemory } from "./instruction-requirements";
import { DecodedInstruction } from "../instructions/instruction";
import { Countdown } from "../util/countdown";
import { HasRegisters, HasMemory } from "./register-file";

class IdleUnit { }

class ActiveUnit<ReadRequirements extends ReadRequirement, WriteRequirements extends WriteRequirement, DataSource, Result extends ExecutionResult> {
  constructor(readonly rf: DataSource, readonly pc: PC, readonly instruction: DecodedInstruction<ReadRequirements, WriteRequirements, DataSource, Result>, readonly resultsHandlers: ExecutionResultsHandler[]) { }
}

type ExecutionUnitState<ReadRequirements extends ReadRequirement, WriteRequirements extends WriteRequirement, DataSource, Result extends ExecutionResult> = IdleUnit | ActiveUnit<ReadRequirements, WriteRequirements, DataSource, Result>;

export class ExecutionUnit<ReadRequirements extends ReadRequirement, WriteRequirements extends WriteRequirement, DataSource, Result extends ExecutionResult> extends Countdown {
  private _state: ExecutionUnitState<ReadRequirements, WriteRequirements, DataSource, Result>;

  constructor() {
    super();

    this._state = new IdleUnit();
  }

  get isAvailable() {
    return this._state instanceof IdleUnit;
  }

  executeInstruction(rf: DataSource, pc: PC, instruction: DecodedInstruction<ReadRequirements, WriteRequirements, DataSource, Result>, resultsHandlers: ExecutionResultsHandler[]) {
    if (this.isAvailable) {
      this._state = new ActiveUnit<ReadRequirements, WriteRequirements, DataSource, Result>(rf, pc, instruction, resultsHandlers);
      this.reset(instruction.duration);
    } else {
      throw Error("Execution Unit not ready!");
    }
  }

  onCompletion(): void {
    if (this._state instanceof ActiveUnit) {
      const instructionResults = this._state.instruction.execute(this._state.rf, this._state.pc);

      for (let index = 0; index < this._state.resultsHandlers.length; index++) {
        this._state.resultsHandlers[index].handleExecutionResults(instructionResults);
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

export type ArithmeticExecutionUnit = ExecutionUnit<ReadsRegister, SetsRegister, HasRegisters, RegisterWriter>;
export type MemoryExecutionUnit = ExecutionUnit<ReadsRegister | ReadsFromMemory, SetsRegister | WritesToMemory, HasRegisters | HasMemory, RegisterWriter | MemoryWriter>;
export type BranchExecutionUnit = ExecutionUnit<ReadsRegister, SetsRegister, HasRegisters, RegisterWriter | BranchPredictionError>;
export type IOExecutionUnit = ExecutionUnit<ReadsRegister, SetsRegister, HasRegisters, RegisterWriter | ExternalAction>;
export type MiscExecutionUnit = ExecutionUnit<never, never, never, Halter>;









