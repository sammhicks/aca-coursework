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

  reset() {
    this.currentState.reset();
    this.futureState.reset();
    this.readersState.reset();
  }
}

export interface RegisterSync {
  getRegisterSync(reg: Register): RegisterFileItemSync;
}

export type MemorySlot = number;

export interface MemorySync {
  mapAddress(addr: Address): MemorySlot;

  getMemorySync(addr: Address): RegisterFileItemSync;

  mapMemorySyncs<T>(action: (sync: RegisterFileItemSync, slot: MemorySlot) => T): T[];
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

  mapAddress(addr: Address) { return addr % this._memorySync.length; }

  getMemorySync(addr: Address) { return this._memorySync[this.mapAddress(addr)]; }

  mapMemorySyncs<T>(action: (sync: RegisterFileItemSync, slot: MemorySlot) => T) {
    return this._memorySync.map(action);
  }

  reset() {
    this._registerSync.forEach(s => s.reset());
    this._memorySync.forEach(s => s.reset());
  }
}
