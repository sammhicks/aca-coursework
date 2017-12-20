import { ExecutionResult, ExecutionResultsHandler } from "./execution-result";
import { WritableRegisterFile } from "./register-file";

class ReservedSlot { };

class CompleteSlot {
  constructor(readonly results: ExecutionResult[], readonly duration: number) { }
};

type ReorderBufferSlotState = ReservedSlot | CompleteSlot;

export class ReorderBufferSlot implements ExecutionResultsHandler {
  private _state: ReorderBufferSlotState;

  constructor() {
    this._state = new ReservedSlot();
  }

  handleExecutionResults(results: ExecutionResult[], duration: number) {
    if (this._state instanceof ReservedSlot) {
      this._state = new CompleteSlot(results, duration);
    }
    else {
      throw Error("Reorder Buffer is not reserved");
    }
  }

  writeBackIfReady(rf: WritableRegisterFile): [boolean, number] {
    if (this._state instanceof CompleteSlot) {
      this._state.results.forEach(r => r.consume(rf));

      return [true, this._state.duration];
    }

    return [false, 0];
  }
}

export class ReorderBuffer {
  private _rf: WritableRegisterFile;
  private _slots: ReorderBufferSlot[];
  private _aborted: boolean;

  constructor(rf: WritableRegisterFile) {
    this._rf = rf;
    this._slots = [];
    this._aborted = false;
  }

  newSlot(): ReorderBufferSlot {
    const newSlot = new ReorderBufferSlot();

    this._slots.push(newSlot);

    return newSlot;
  }

  writeBack(): [number, number] {
    var writeBackCount = 0;
    var totalDuration = 0;
    this._aborted = false;

    while (this._aborted == false && this._slots.length > 0) {
      const successAndDuration = this._slots[0].writeBackIfReady(this._rf);

      if (successAndDuration[0]) {
        ++writeBackCount;
        totalDuration += successAndDuration[1];
        this._slots.shift();
      }
      else {
        break;
      }
    }

    return [writeBackCount, totalDuration];
  }

  reset() {
    this._slots = [];
    this._aborted = true;
  }
}
