import { Address, PC, Register, Literal, Registers, Memory } from "./basic-types";

export const LR_INDEX = 0;

export interface HasRegisterFileComponents { }

export interface HasPC extends HasRegisterFileComponents {
  getPC(): PC;
}

export interface HasRegisters extends HasRegisterFileComponents {
  getRegister(reg: Register): Literal;
}

export interface HasMemory extends HasRegisterFileComponents {
  readMemory(addr: Address): Literal;
}

export interface ReadableRegisterFile extends HasPC, HasRegisters, HasMemory { }

export function getRegisters(rf: HasRegisters, regs: Register[]) { return regs.map(reg => rf.getRegister(reg)); }

export interface HasWritablePC {
  updatePC(pc: PC): void;
  releasePC(): void;
}

export interface HasWritableRegisters {
  updateRegister(reg: Register, val: Literal): void;
  releaseRegister(reg: Register): void;
}

export interface HasWritableMemory {
  writeMemory(addr: Address, val: Literal): void;
  releaseMemory(addr: Address): void;
}

export interface PerformsExternalActions {
  performExternalAction(action: () => void): void;
}

export interface Halts {
  halt(): void;
}

export interface WritableRegisterFile extends HasWritablePC, HasWritableRegisters, HasWritableMemory, PerformsExternalActions, Halts {
}


export class RegisterFile implements ReadableRegisterFile, WritableRegisterFile {
  private _pc: PC;
  private _registers: Registers;
  private _memory: Memory;

  constructor() {
    this._pc = 0;
    this._registers = [];
    this._memory = [];
  }

  getPC() { return this._pc; }
  updatePC(pc: PC) { this._pc = pc; }
  releasePC() { }

  getRegister(reg: Register) { return this._registers[reg]; }
  updateRegister(reg: Register, val: Literal) { this._registers[reg] = val; }
  releaseRegister(reg: Register) { }

  readMemory(addr: Address) { return this._memory[addr]; }
  writeMemory(addr: Address, val: Literal) { this._memory[addr] = val; }
  releaseMemory(addr: Address) { }

  performExternalAction(action: () => void) { action(); }

  halt() { }
}
