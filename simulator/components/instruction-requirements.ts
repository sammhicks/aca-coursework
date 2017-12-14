import { Register, Literal } from "./basic-types";
import { ExecutionResult, RegisterReleaser, MemoryReleaser } from "./execution-result";
import { HasRegisters, HasMemory, getRegisters } from "./register-file";
import { RegisterFileItemSync, RegisterSync, MemorySlot, MemorySync } from "./register-file-sync";
import { areEqual } from "../util/compare";
import { Semaphore } from "../util/semaphore";

export interface InstructionRequirement {
  updateSync(): void;

  isMet(): boolean;
}

export interface ReadRequirement extends InstructionRequirement {
  getResult(): ExecutionResult;
}

export interface WriteRequirement extends InstructionRequirement { }


abstract class RegisterInteraction {
  protected _sync: RegisterFileItemSync;
  protected _target: Semaphore;
  protected _alreadyUpdatedSync: boolean;

  constructor(sync: RegisterSync, readonly reg: Register) {
    this._sync = sync.getRegisterSync(reg);
    this._target = sync.getRegisterSync(reg).futureState;
    this._alreadyUpdatedSync = false;
  }

  isMet() { return areEqual(this._sync.currentState, this._target); }
}

export class ReadsRegister extends RegisterInteraction implements ReadRequirement {
  updateSync() {
    if (!this._alreadyUpdatedSync && this.isMet()) {
      this._alreadyUpdatedSync;
      this._sync.readersCount += 1;
    }
  }

  getResult() { return new RegisterReleaser(this.reg); }
}

export class SetsRegister extends RegisterInteraction implements WriteRequirement {
  updateSync() {
    if (!this._alreadyUpdatedSync) {
      this._alreadyUpdatedSync = true;
      this._sync.futureState.increment();
    }
  }

  isMet() { return super.isMet() && this._sync.readersCount == 0; }
}

abstract class BaseMemoryRequirement {
  protected _rf: HasRegisters & HasMemory;
  protected _regReqs: ReadsRegister[];
  protected _sync: MemorySync;
  protected _target: Semaphore[];
  protected _updatedSyncs: boolean[];

  constructor(sync: RegisterSync & MemorySync, rf: HasRegisters & HasMemory, readonly addrRegs: Register[], readonly addrOffset: Literal) {
    this._rf = rf;
    this._regReqs = addrRegs.map(reg => new ReadsRegister(sync, reg));
    this._sync = sync;
    this._target = sync.mapMemorySyncs(sync => sync.futureState);
    this._updatedSyncs = sync.mapMemorySyncs(() => false);
  }

  protected getAddress() { return getRegisters(this._rf, this.addrRegs).reduce((acc, item) => acc + item, this.addrOffset); }

  protected getMemorySlot() { return this._sync.mapAddress(this.getAddress()); }

  isMet() { return this._regReqs.every(req => req.isMet()) && areEqual(this._sync.getMemorySync(this.getMemorySlot()).currentState, this._target[this.getMemorySlot()]); }
}

export class ReadsFromMemory extends BaseMemoryRequirement implements ReadRequirement {
  updateSync() {
    this._regReqs.forEach(req => req.updateSync());

    // If we know what the address is
    if (this._regReqs.every(sync => sync.isMet())) {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (self._updatedSyncs[slot]) {
          self._updatedSyncs[slot] = false;
          if (slot == self.getMemorySlot()) {
            sync.readersCount -= 1;
          }
        }
      });
    } else {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (!self._updatedSyncs[slot] && areEqual(self._sync.getMemorySync(slot).currentState, self._target[slot])) {
          self._updatedSyncs[slot] = true;
          sync.readersCount += 1;
        }
      });
    }
  }

  getResult() { return new MemoryReleaser(this.getAddress()); }
}

export class WritesToMemory extends BaseMemoryRequirement {
  updateSync() {
    this._regReqs.forEach(req => req.updateSync());

    // If we know what the address is
    if (this._regReqs.every(sync => sync.isMet())) {
      const self = this;
      this._sync.mapMemorySyncs(function (sync: RegisterFileItemSync, slot: MemorySlot) {
        if (self._updatedSyncs[slot]) {
          self._updatedSyncs[slot] = false;
          if (slot != self.getMemorySlot()) {
            sync.currentState.increment();
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

  isMet() { return super.isMet() && this._sync.getMemorySync(this.getMemorySlot()).readersCount == 0; }
}
