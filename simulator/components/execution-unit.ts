import { Instruction } from "../instructions/instruction";
import { RegisterFile } from "./register-file";
import { ReorderBufferSlot } from "./reorder-buffer";
import { Countdown } from "../util";

interface ExecutionUnitState { }

class IdleUnit implements ExecutionUnitState { }

class ActiveUnit implements ExecutionUnitState {
  constructor(public rf: RegisterFile, public instruction: Instruction, public reorderBufferSlot: ReorderBufferSlot) { }
}

export class ExecutionUnit extends Countdown {
  private _state: ExecutionUnitState;

  constructor() {
    super();

    this._state = new IdleUnit();
  }

  get isAvailable() {
    return this._state instanceof IdleUnit;
  }

  executeInstruction(rf: RegisterFile, instruction: Instruction, reorderBufferSlot: ReorderBufferSlot) {
    if (this._state instanceof IdleUnit) {
      this._state = new ActiveUnit(rf, instruction, reorderBufferSlot);
      reorderBufferSlot.updateInstructionHandler(this);
      this.reset(instruction.duration());
    } else {
      throw Error("Execution Unit not ready!");
    }
  }

  onCompletion(): void {
    if (this._state instanceof ActiveUnit) {
      const instructionResults = this._state.instruction.execute(this._state.rf);

      if (Instruction.isSuccessfulExecution(instructionResults)) {
        this._state.reorderBufferSlot.buffer(instructionResults);
      } else {
        this._state.reorderBufferSlot.buffer(instructionResults.writes, true);
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
