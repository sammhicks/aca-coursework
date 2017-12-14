import { Address, PC, Register, Literal } from "./basic-types";
import { initArray } from "../util/init-array";
import { Semaphore } from "../util/semaphore";
import { areEqual } from "../util/compare";

export class RegisterFileItemSync {
  public currentState: Semaphore;
  public futureState: Semaphore;
  public readersCount: number;

  constructor() {
    this.currentState = new Semaphore(0);
    this.futureState = new Semaphore(0);
    this.readersCount = 0;
  }
}

export interface PCSync {
  getPCSync(): RegisterFileItemSync;

  resetPCReadersCount(): void;
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

export class RegisterFileSync implements PCSync, RegisterSync, MemorySync {
  private _pcSync: RegisterFileItemSync;
  private _registerSync: RegisterFileItemSync[];
  private _memorySync: RegisterFileItemSync[];

  constructor(memorySlots: number) {
    this._pcSync = new RegisterFileItemSync();
    this._registerSync = [];
    this._memorySync = initArray(memorySlots, () => new RegisterFileItemSync());
  }

  getPCSync() { return this._pcSync; }

  resetPCReadersCount() { this._pcSync.readersCount = 0; }


  getRegisterSync(reg: Register) {
    if (!(reg in this._registerSync)) {
      this._registerSync[reg] = new RegisterFileItemSync();
    }

    return this._registerSync[reg];
  }

  resetRegisterReadersCount() {
    for (let index = 0; index < this._registerSync.length; index++) {
      this._registerSync[index].readersCount = 0;
    }
  }


  mapAddress(addr: Address) { return addr % this._memorySync.length; }

  getMemorySync(addr: Address) { return this._memorySync[this.mapAddress(addr)]; }

  mapMemorySyncs<T>(action: (sync: RegisterFileItemSync, slot: MemorySlot) => T) {
    return this._memorySync.map(action);
  }

  resetMemoryReadersCount() {
    for (let index = 0; index < this._memorySync.length; index++) {
      this.getMemorySync(index).readersCount = 0;
    }
  }

  resetReadersCount() {
    this.resetPCReadersCount();
    this.resetRegisterReadersCount();
    this.resetMemoryReadersCount();
  }
}
