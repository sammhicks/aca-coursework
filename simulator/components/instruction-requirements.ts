import { Register, Literal } from "./basic-types";
import { HasRegisters, HasMemory, getRegisters } from "./register-file";
import { RegisterFileItemSync, RegisterSync, MemorySlot, MemorySync } from "./register-file-sync";
import { areEqual } from "../util/compare";
import { Semaphore } from "../util/semaphore";

export interface InstructionRequirement {
  updateSync(): void;

  isMet(): boolean;

  releaseUnused(): void;
}

export abstract class RegisterRequirement implements InstructionRequirement {
  protected _sync: RegisterFileItemSync;
  protected _target: Semaphore;
  protected _alreadyUpdatedSync: boolean;

  constructor(sync: RegisterSync, readonly reg: Register) {
    this._sync = sync.getRegisterSync(reg);
    this._target = sync.getRegisterSync(reg).futureState.clone();
    this._alreadyUpdatedSync = false;
  }

  abstract updateSync(): void;

  isMet() { return areEqual(this._sync.currentState, this._target); }

  releaseUnused() { }
}

export class ReadsRegister extends RegisterRequirement {
  updateSync() {
    if (!this._alreadyUpdatedSync && this.isMet()) {
      this._alreadyUpdatedSync = true;
      this._sync.readersState.increment();
    }
  }
}

export class SetsRegister extends RegisterRequirement {
  updateSync() {
    if (!this._alreadyUpdatedSync) {
      this._alreadyUpdatedSync = true;
      this._sync.futureState.increment();
    }
  }

  isMet() { return super.isMet() && this._sync.readersState.isZero(); }
}

export function registerInteractions(sync: RegisterSync, dst: Register | null, src: (Register | null)[]): RegisterRequirement[] {

  return ([] as RegisterRequirement[])
    .concat((src.filter(r => (r != null && r != undefined && r != dst)) as Register[]).map(r => new ReadsRegister(sync, r)))
    .concat(dst == null ? [] : [new SetsRegister(sync, dst)]);
}

export abstract class MemoryRequirement implements InstructionRequirement {
  protected _rf: HasRegisters & HasMemory;
  protected _regReqs: ReadsRegister[];
  protected _sync: MemorySync;
  protected _target: Semaphore[];
  protected _updatedSyncs: boolean[];

  constructor(sync: RegisterSync & MemorySync, rf: HasRegisters & HasMemory, readonly dst: Register | null, readonly addrRegs: Register[], readonly addrOffset: Literal) {
    this._rf = rf;
    this._regReqs = addrRegs.filter(r => r != dst).map(reg => new ReadsRegister(sync, reg));
    this._sync = sync;
    this._target = sync.mapMemorySyncs(sync => sync.futureState.clone());
    this._updatedSyncs = sync.mapMemorySyncs(() => false);
  }

  protected getAddress() { return getRegisters(this._rf, this.addrRegs).reduce((acc, item) => acc + item, this.addrOffset); }

  protected getMemorySlot() { return this._sync.mapAddress(this.getAddress()); }

  abstract updateSync(): void;

  isMet() { return this._regReqs.every(req => req.isMet()) && areEqual(this._sync.getMemorySync(this.getMemorySlot()).currentState, this._target[this.getMemorySlot()]); }

  abstract releaseUnused(): void;
}

export class ReadsFromMemory extends MemoryRequirement {
  updateSync() {
    this._regReqs.forEach(req => req.updateSync());

    // If we know what the address is
    if (this._regReqs.every(sync => sync.isMet())) {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (slot == self.getMemorySlot()) {
          // Increment the used memory slot reader
          if (!self._updatedSyncs[slot] && areEqual(sync.currentState, self._target[slot])) {
            self._updatedSyncs[slot] = true;
            sync.readersState.increment();
          }
        }
      });
    } else {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (!self._updatedSyncs[slot] && areEqual(sync.currentState, self._target[slot])) {
          self._updatedSyncs[slot] = true;
          sync.readersState.increment();
        }
      });
    }
  }

  releaseUnused() {
    this._regReqs.forEach(req => req.releaseUnused());

    // If we know what the address is
    if (this._regReqs.every(sync => sync.isMet())) {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (slot != self.getMemorySlot()) {
          // If we've incremented the reader, decrement it
          if (self._updatedSyncs[slot]) {
            if (!areEqual(sync.currentState, self._target[slot])) {
              throw Error("Memory Read gone wrong!");
            }
            self._updatedSyncs[slot] = false;
            sync.readersState.decrement();
          }
        }
      });
    }
  }
}

export class WritesToMemory extends MemoryRequirement {
  updateSync() {
    this._regReqs.forEach(req => req.updateSync());

    // If we know what the address is
    if (this._regReqs.every(sync => sync.isMet())) {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (slot == self.getMemorySlot()) {
          if (!self._updatedSyncs[slot]) {
            self._updatedSyncs[slot] = true;
            sync.futureState.increment();
          }
        }
      });
    } else {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (!self._updatedSyncs[slot]) {
          self._updatedSyncs[slot] = true;
          sync.futureState.increment();
        }
      });
    }
  }

  isMet() { return super.isMet() && this._sync.getMemorySync(this.getMemorySlot()).readersState.isZero(); }

  releaseUnused() {
    this._regReqs.forEach(req => req.releaseUnused());

    // If we know what the address is
    if (this._regReqs.every(sync => sync.isMet())) {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (slot != self.getMemorySlot()) {
          if (self._updatedSyncs[slot]) {
            self._updatedSyncs[slot] = false;
            sync.currentState.increment();
          }
        }
      });
    }
  }
}
