import { PC } from "./basic-types";
import { ExecutionResult, RegisterWriter, RegisterReleaser, MemoryWriter, MemoryReleaser, ExternalAction, Halter, BranchPredictionError, ExecutionResultsHandler, TookBranch, Returned } from "./execution-result";
import { InstructionRequirement, RegisterRequirement, MemoryRequirement } from "./instruction-requirements";
import { HasRegisters, HasMemory, RegisterFile } from "./register-file";
import { Instruction, DecodedInstruction, ArithmeticInstruction, MemoryInstruction, BranchInstruction, IOInstruction, MiscInstruction } from "../instructions/instruction";
import { initArray } from "../util/init-array";
import { Countdown } from "../util/countdown";
import { Prediction } from "./prediction";

class IdleUnit { }

class ActiveUnit<InstructionRequirements extends InstructionRequirement, DataSource, Result extends ExecutionResult> {
  readonly expectedPC: PC;

  constructor(readonly rf: DataSource, prediction: Prediction, readonly pc: PC, readonly instruction: DecodedInstruction<InstructionRequirements, DataSource, Result>, readonly resultsHandlers: ExecutionResultsHandler[]) {
    this.expectedPC = instruction.expectedPC(pc, prediction);
  }
}

type ExecutionUnitState<InstructionRequirements extends InstructionRequirement, DataSource, Result extends ExecutionResult> = IdleUnit | ActiveUnit<InstructionRequirements, DataSource, Result>;

export class ExecutionUnit<InstructionRequirements extends InstructionRequirement, DataSource, Result extends ExecutionResult> extends Countdown {
  private _state: ExecutionUnitState<InstructionRequirements, DataSource, Result>;

  constructor() {
    super();

    this._state = new IdleUnit();
  }

  get isAvailable() {
    return this._state instanceof IdleUnit;
  }

  executeInstruction(rf: DataSource, prediction: Prediction, pc: PC, instruction: DecodedInstruction<InstructionRequirements, DataSource, Result>, resultsHandlers: ExecutionResultsHandler[]) {
    if (this.isAvailable) {
      this._state = new ActiveUnit<InstructionRequirements, DataSource, Result>(rf, prediction, pc, instruction, resultsHandlers);
      this.reset(instruction.duration);
    } else {
      throw Error("Execution Unit not ready!");
    }
  }

  onCompletion(): void {
    if (this._state instanceof ActiveUnit) {
      const instructionResults = this._state.instruction.execute(this._state.rf, this._state.pc, this._state.expectedPC);

      this._state.resultsHandlers.forEach(handler => handler.handleExecutionResults(instructionResults));

      this._state = new IdleUnit();
    } else {
      throw Error("Completed Execution Unit has no instruction!");
    }
  }

  onAborted(): void { this._state = new IdleUnit(); }
}

export class ArithmeticExecutionUnit extends ExecutionUnit<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser>{ }
export class MemoryExecutionUnit extends ExecutionUnit<RegisterRequirement | MemoryRequirement, HasRegisters | HasMemory, RegisterWriter | RegisterReleaser | MemoryWriter | MemoryReleaser>{ }
export class BranchExecutionUnit extends ExecutionUnit<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser | TookBranch | Returned | BranchPredictionError>{ }
export class IOExecutionUnit extends ExecutionUnit<RegisterRequirement, HasRegisters, RegisterWriter | RegisterReleaser | ExternalAction> { }
export class MiscExecutionUnit extends ExecutionUnit<never, never, Halter> { }

export class ExecutionUnits {
  private _prediction: Prediction;
  private _executionUnits: Countdown[];
  private _arithmeticExecutionUnits: ArithmeticExecutionUnit[];
  private _memoryExecutionUnits: MemoryExecutionUnit[];
  private _branchExecutionUnit: BranchExecutionUnit;
  private _ioExecutionUnit: IOExecutionUnit;
  private _miscExecutionUnit: MiscExecutionUnit;

  constructor(arithmeticExecutionUnitCount: number, memoryExecutionUnitCount: number, prediction: Prediction) {
    this._prediction = prediction;
    this._arithmeticExecutionUnits = initArray(arithmeticExecutionUnitCount, () => new ArithmeticExecutionUnit());
    this._memoryExecutionUnits = initArray(memoryExecutionUnitCount, () => new MemoryExecutionUnit());
    this._branchExecutionUnit = new BranchExecutionUnit();
    this._ioExecutionUnit = new IOExecutionUnit();
    this._miscExecutionUnit = new MiscExecutionUnit();
    this._executionUnits = ([] as Countdown[])
      .concat(this._arithmeticExecutionUnits)
      .concat(this._memoryExecutionUnits)
      .concat([this._branchExecutionUnit, this._ioExecutionUnit, this._miscExecutionUnit]);
  }

  executeInstruction(rf: RegisterFile, pc: PC, instruction: Instruction, resultsHandlers: ExecutionResultsHandler[]): boolean {
    if (instruction instanceof ArithmeticInstruction) {
      for (let index = 0; index < this._arithmeticExecutionUnits.length; index++) {
        const executionUnit = this._arithmeticExecutionUnits[index];
        if (executionUnit.isAvailable) {
          executionUnit.executeInstruction(rf, this._prediction, pc, instruction, resultsHandlers);

          return true;
        }
      }

      return false;
    } else if (instruction instanceof MemoryInstruction) {
      for (let index = 0; index < this._memoryExecutionUnits.length; index++) {
        const executionUnit = this._memoryExecutionUnits[index];
        if (executionUnit.isAvailable) {
          executionUnit.executeInstruction(rf, this._prediction, pc, instruction, resultsHandlers);

          return true;
        }
      }

      return false;
    } else if (instruction instanceof BranchInstruction) {
      if (this._branchExecutionUnit.isAvailable) {
        this._branchExecutionUnit.executeInstruction(rf, this._prediction, pc, instruction, resultsHandlers);

        return true;
      }

      return false;
    } else if (instruction instanceof IOInstruction) {
      if (this._ioExecutionUnit.isAvailable) {
        this._ioExecutionUnit.executeInstruction(rf, this._prediction, pc, instruction, resultsHandlers);

        return true;
      }

      return false;
    } else if (instruction instanceof MiscInstruction) {
      if (this._miscExecutionUnit.isAvailable) {
        this._miscExecutionUnit.executeInstruction(rf as never, this._prediction, pc, instruction, resultsHandlers);

        return true;
      }

      return false;
    }
    return false;
  }

  tick() { this._executionUnits.forEach(unit => unit.tick()); }

  resetUnits() { this._executionUnits.forEach(unit => unit.abort()); }
}
