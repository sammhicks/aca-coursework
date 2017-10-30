import { RegisterFile, RegisterWriter } from "./register-file";
import { Counter } from "../util/counter";

enum ReorderBufferSlotState {
  Available,
  Reserved,
  Complete
};

export class ReorderBufferSlot {
  private _state: ReorderBufferSlotState;
  private _writes: RegisterWriter[];

  constructor() {
    this._state = ReorderBufferSlotState.Available;
    this._writes = [];
  }

  get isAvailable() {
    return this._state == ReorderBufferSlotState.Available;
  }

  get isReserved() {
    return this._state == ReorderBufferSlotState.Reserved;
  }

  get isComplete() {
    return this._state == ReorderBufferSlotState.Complete;
  }

  reserve() {
    if (this.isAvailable) {
      this._state = ReorderBufferSlotState.Reserved
    } else {
      throw Error("Reorder Buffer is not available");
    }
  }

  buffer(writes: RegisterWriter[]) {
    if (this.isReserved) {
      this._state = ReorderBufferSlotState.Complete;
      this._writes = writes;
    }
    else {
      throw Error("Reorder Buffer is not reserved");
    }
  }

  clear() {
    this._state = ReorderBufferSlotState.Available;
    this._writes = [];
  }

  writeBackIfReady(rf: RegisterFile): boolean {
    if (this.isComplete) {
      for (var index = 0; index < this._writes.length; index++) {
        this._writes[index].write(rf);
      }

      this._state = ReorderBufferSlotState.Available;
      this._writes = [];

      return true;
    }

    return false;
  }
}

export class ReorderBuffer {
  private _rf: RegisterFile;
  private _slots: ReorderBufferSlot[];
  private _freeSlotIndex: Counter;
  private _activeSlotIndex: Counter;

  constructor(rf: RegisterFile, size: number) {
    this._rf = rf;
    this._slots = [];

    for (var n = 0; n < size; ++n) {
      this._slots[n] = new ReorderBufferSlot();
    }

    this._freeSlotIndex = new Counter(size);
    this._activeSlotIndex = new Counter(size);
  }

  get freeSlot() {
    return this._slots[this._freeSlotIndex.value];
  }

  get activeSlot() {
    return this._slots[this._activeSlotIndex.value];
  }

  newSlot(): ReorderBufferSlot {
    const freeSlot = this.freeSlot;
    if (freeSlot.isAvailable) {
      freeSlot.reserve();
      this._activeSlotIndex.increment();

      return freeSlot;
    } else {
      throw Error("No free slot");
    }
  }

  writeBack() {
    while (this.activeSlot.writeBackIfReady(this._rf) {
      this._activeSlotIndex.increment();
    }
  }
}
