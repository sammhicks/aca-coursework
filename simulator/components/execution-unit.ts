import { PC } from "./basic-types";
import { ExecutionResult, RegisterWriter, MemoryWriter, ExternalAction, Halter, BranchPredictionError, ExecutionResultsHandler } from "./execution-result";
import { InstructionRequirement, RegisterRequirement, MemoryRequirement } from "./instruction-requirements";
import { Instruction, DecodedInstruction, ArithmeticInstruction, MemoryInstruction, BranchInstruction, IOInstruction, MiscInstruction } from "../instructions/instruction";
import { Countdown } from "../util/countdown";
import { HasRegisters, HasMemory, HandlesBranchPredictionError, RegisterFile } from "./register-file";
import { initArray } from "../util/init-array";

class IdleUnit { }

class ActiveUnit<InstructionRequirements extends InstructionRequirement, DataSource, Result extends ExecutionResult> {
  constructor(readonly rf: DataSource, readonly pc: PC, readonly instruction: DecodedInstruction<InstructionRequirements, DataSource, Result>, readonly resultsHandlers: ExecutionResultsHandler[]) { }
}

type ExecutionUnitState<InstructionRequirements extends InstructionRequirement, DataSource, Result extends ExecutionResult> = IdleUnit | ActiveUnit<InstructionRequirements, DataSource, Result>;

export class ExecutionUnit<InstructionRequirements extends InstructionRequirement, DataSource, Result extends ExecutionResult> extends Countdown implements HandlesBranchPredictionError {
  private _state: ExecutionUnitState<InstructionRequirements, DataSource, Result>;

  constructor() {
    super();

    this._state = new IdleUnit();
  }

  get isAvailable() {
    return this._state instanceof IdleUnit;
  }

  executeInstruction(rf: DataSource, pc: PC, instruction: DecodedInstruction<InstructionRequirements, DataSource, Result>, resultsHandlers: ExecutionResultsHandler[]) {
    if (this.isAvailable) {
      this._state = new ActiveUnit<InstructionRequirements, DataSource, Result>(rf, pc, instruction, resultsHandlers);
      this.reset(instruction.duration);
    } else {
      throw Error("Execution Unit not ready!");
    }
  }

  onCompletion(): void {
    if (this._state instanceof ActiveUnit) {
      const instructionResults = this._state.instruction.execute(this._state.rf, this._state.pc);

      this._state.resultsHandlers.forEach(handler => handler.handleExecutionResults(instructionResults));

      this._state = new IdleUnit();
    } else {
      throw Error("Completed Execution Unit has no instruction!");
    }
  }

  onAborted(): void { this._state = new IdleUnit(); }

  handleBranchPredictionError() { this.abort(); }
}

export class ArithmeticExecutionUnit extends ExecutionUnit<RegisterRequirement, HasRegisters, RegisterWriter>{ }
export class MemoryExecutionUnit extends ExecutionUnit<RegisterRequirement | MemoryRequirement, HasRegisters | HasMemory, RegisterWriter | MemoryWriter>{ }
export class BranchExecutionUnit extends ExecutionUnit<RegisterRequirement, HasRegisters, RegisterWriter | BranchPredictionError>{ }
export class IOExecutionUnit extends ExecutionUnit<RegisterRequirement, HasRegisters, RegisterWriter | ExternalAction> { }
export class MiscExecutionUnit extends ExecutionUnit<never, never, Halter> { }

export class ExecutionUnits {
  private _arithmeticExecutionUnits: ArithmeticExecutionUnit[];
  private _memoryExecutionUnits: MemoryExecutionUnit[];
  private _branchExecutionUnit: BranchExecutionUnit;
  private _ioExecutionUnit: IOExecutionUnit;
  private _miscExecutionUnit: MiscExecutionUnit;

  constructor(arithmeticExecutionUnitCount: number, memoryExecutionUnitCount: number) {
    this._arithmeticExecutionUnits = initArray(arithmeticExecutionUnitCount, () => new ArithmeticExecutionUnit());
    this._memoryExecutionUnits = initArray(memoryExecutionUnitCount, () => new MemoryExecutionUnit());
    this._branchExecutionUnit = new BranchExecutionUnit();
    this._ioExecutionUnit = new IOExecutionUnit();
    this._miscExecutionUnit = new MiscExecutionUnit();
  }

  executeInstruction(rf: RegisterFile, pc: PC, instruction: Instruction, resultsHandlers: ExecutionResultsHandler[]): boolean {
    if (instruction instanceof ArithmeticInstruction) {
      for (let index = 0; index < this._arithmeticExecutionUnits.length; index++) {
        const executionUnit = this._arithmeticExecutionUnits[index];
        if (executionUnit.isAvailable) {
          executionUnit.executeInstruction(rf, pc, instruction, resultsHandlers);

          return true;
        }
      }

      return false;
    } else if (instruction instanceof MemoryInstruction) {
      for (let index = 0; index < this._memoryExecutionUnits.length; index++) {
        const executionUnit = this._memoryExecutionUnits[index];
        if (executionUnit.isAvailable) {
          executionUnit.executeInstruction(rf, pc, instruction, resultsHandlers);

          return true;
        }
      }

      return false;
    } else if (instruction instanceof BranchInstruction) {
      if (this._branchExecutionUnit.isAvailable) {
        this._branchExecutionUnit.executeInstruction(rf, pc, instruction, resultsHandlers);

        return true;
      }

      return false;
    } else if (instruction instanceof IOInstruction) {
      if (this._ioExecutionUnit.isAvailable) {
        this._ioExecutionUnit.executeInstruction(rf, pc, instruction, resultsHandlers);

        return true;
      }

      return false;
    } else if (instruction instanceof MiscInstruction) {
      if (this._miscExecutionUnit.isAvailable) {
        this._miscExecutionUnit.executeInstruction(rf as never, pc, instruction, resultsHandlers);

        return true;
      }

      return false;
    }
  }
}
