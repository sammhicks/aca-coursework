import { RegisterFile, RegisterFileWriter } from "./register-file";
import { Abortable, Counter, initArray, RingItem } from "../util";

interface ReorderBufferSlotState { };

class AvailableSlot implements ReorderBufferSlotState { };

class ReservedSlot implements ReorderBufferSlotState {
  constructor(public instructionHandler: Abortable) { }
};

class CompleteSlot implements ReorderBufferSlotState {
  constructor(public writes: RegisterFileWriter[]) { }
};

export class ReorderBufferSlot extends RingItem<ReorderBufferSlot> {
  private _resetBuffer: (slot: ReorderBufferSlot) => void;
  private _state: ReorderBufferSlotState;

  constructor(resetBuffer: (slot: ReorderBufferSlot) => void) {
    super();
    this._resetBuffer = resetBuffer;
    this._state = {};
  }

  get isAvailable() {
    return this._state instanceof AvailableSlot;
  }

  get isReserved() {
    return this._state instanceof ReservedSlot;
  }

  get isComplete() {
    return this._state instanceof CompleteSlot;
  }

  reserve(instructionHandler: Abortable) {
    if (this._state instanceof AvailableSlot) {
      this._state = new ReservedSlot(instructionHandler);
    } else {
      throw Error("Reorder Buffer is not available");
    }
  }

  updateInstructionHandler(instructionHandler: Abortable) {
    if (this._state instanceof ReservedSlot) {
      this._state.instructionHandler = instructionHandler;
    }
  }

  buffer(writes: RegisterFileWriter[], clearSuccessors: boolean = false) {
    if (this._state instanceof ReservedSlot) {
      this._state = new CompleteSlot(writes);

      if (clearSuccessors) {
        this._resetBuffer(this.next);
        this.next.clearRipple();
      }
    }
    else {
      throw Error("Reorder Buffer is not reserved");
    }
  }

  private clearRipple() {
    if (!(this._state instanceof AvailableSlot)) {
      if (this._state instanceof ReservedSlot) {
        this._state.instructionHandler.abort();
      }

      this._state = new AvailableSlot();
    }
  }

  writeBackIfReady(rf: RegisterFile): boolean {
    if (this._state instanceof CompleteSlot) {
      for (var index = 0; index < this._state.writes.length; index++) {
        this._state.writes[index].write(rf);
      }

      this._state = new AvailableSlot();

      return true;
    }

    return false;
  }
}

export class ReorderBuffer {
  private _rf: RegisterFile;
  private _freeSlot: ReorderBufferSlot;
  private _activeSlot: ReorderBufferSlot;

  constructor(rf: RegisterFile, size: number) {
    this._rf = rf;

    const self = this;
    this._freeSlot = RingItem.createRing(size, () => new ReorderBufferSlot(function resetBuffer(slot: ReorderBufferSlot) {
      self._freeSlot = slot;
      self._activeSlot = slot;
    }));
    this._activeSlot = this._freeSlot;
  }

  newSlot(): ReorderBufferSlot {
    const freeSlot = this._freeSlot;
    if (freeSlot.isAvailable) {
      freeSlot.reserve();
      this._freeSlot = this._freeSlot.next;

      return freeSlot;
    } else {
      throw Error("No free slot");
    }
  }

  writeBack(): number {
    var writeBackCount = 0;
    while (this._activeSlot.writeBackIfReady(this._rf)) {
      this._activeSlot = this._activeSlot.next;
      ++writeBackCount;
    }
    return writeBackCount;
  }
}