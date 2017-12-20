import { ExecutionResult, ExecutionResultsHandler } from "./execution-result";
import { WritableRegisterFile } from "./register-file";

class ReservedSlot { };

class CompleteSlot {
  constructor(public results: ExecutionResult[]) { }
};

type ReorderBufferSlotState = ReservedSlot | CompleteSlot;

export class ReorderBufferSlot implements ExecutionResultsHandler {
  private _state: ReorderBufferSlotState;

  constructor() {
    this._state = new ReservedSlot();
  }

  handleExecutionResults(results: ExecutionResult[]) {
    if (this._state instanceof ReservedSlot) {
      this._state = new CompleteSlot(results);
    }
    else {
      throw Error("Reorder Buffer is not reserved");
    }
  }

  writeBackIfReady(rf: WritableRegisterFile): boolean {
    if (this._state instanceof CompleteSlot) {
      this._state.results.forEach(r => r.consume(rf));

      return true;
    }

    return false;
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

  writeBack(): number {
    var writeBackCount = 0;
    this._aborted = false;

    while (this._aborted == false && this._slots.length > 0 && this._slots[0].writeBackIfReady(this._rf)) {
      this._slots.shift();
      ++writeBackCount;
    }

    return writeBackCount;
  }

  reset() {
    this._slots = [];
    this._aborted = true;
  }
}
