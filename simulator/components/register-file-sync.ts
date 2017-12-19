import { Address, Register } from "./basic-types";
import { initArray } from "../util/init-array";
import { Semaphore, BiDirectionSemaphore } from "../util/semaphore";

export class RegisterFileItemSync {
  public currentState: Semaphore;
  public futureState: Semaphore;
  public readersState: BiDirectionSemaphore;

  constructor() {
    this.currentState = new Semaphore();
    this.futureState = new Semaphore();
    this.readersState = new BiDirectionSemaphore();
  }
}

export interface RegisterSync {
  getRegisterSync(reg: Register): RegisterFileItemSync;

  resetRegisterReadersCount(): void;
}

export type MemorySlot = number;

export interface MemorySync {
  mapAddress(addr: Address): MemorySlot;

  getMemorySync(addr: Address): RegisterFileItemSync;

  mapMemorySyncs<T>(action: (sync: RegisterFileItemSync, slot: MemorySlot) => T): T[];

  resetMemoryReadersCount(): void;
}

export class RegisterFileSync implements RegisterSync, MemorySync {
  private _registerSync: RegisterFileItemSync[];
  private _memorySync: RegisterFileItemSync[];

  constructor(memorySlots: number) {
    this._registerSync = [];
    this._memorySync = initArray(memorySlots, () => new RegisterFileItemSync());
  }

  getRegisterSync(reg: Register) {
    if (!(reg in this._registerSync)) {
      this._registerSync[reg] = new RegisterFileItemSync();
    }

    return this._registerSync[reg];
  }

  resetRegisterReadersCount() {
    for (let index = 0; index < this._registerSync.length; index++) {
      this._registerSync[index].readersState.reset();
    }
  }


  mapAddress(addr: Address) { return addr % this._memorySync.length; }

  getMemorySync(addr: Address) { return this._memorySync[this.mapAddress(addr)]; }

  mapMemorySyncs<T>(action: (sync: RegisterFileItemSync, slot: MemorySlot) => T) {
    return this._memorySync.map(action);
  }

  resetMemoryReadersCount() {
    for (let index = 0; index < this._memorySync.length; index++) {
      this.getMemorySync(index).readersState.reset();
    }
  }

  resetReadersCount() {
    this.resetRegisterReadersCount();
    this.resetMemoryReadersCount();
  }
}
