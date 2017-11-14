import { Address, PC, Register, Literal, Memory, Registers } from "./basic-types";

export const LR_INDEX = 0;

export function lookupRegisters(rf: LikeRegisterFile, rs: Register[]): Literal[] { return rs.map(r => rf.readRegister(r)); }

export interface LikeRegisterFile {
  pc: PC;

  readRegister(reg: Register): Literal;
  writeRegister(reg: Register, val: Literal): void;

  readMemory(addr: Address): Literal;
  writeMemory(addr: Address, val: Literal): void;

  performExternalAction(action: () => void): void;

  isRunning(): boolean;
  halt(): void;
}

export class RegisterFile implements LikeRegisterFile {
  private _pc: PC;
  private _registers: Registers;
  private _memory: Memory;
  private _isRunning: boolean;

  constructor() {
    this._pc = 0;
    this._registers = [];
    this._memory = [];
    this._isRunning = true;
  }

  get pc(): PC { return this._pc; }

  set pc(pc: PC) { this._pc = pc; }


  readRegister(r: Register): Literal { return this._registers[r]; }

  writeRegister(reg: Register, val: Literal): void { this._registers[reg] = val; }


  readMemory(addr: Literal) { return this._memory[addr]; }

  writeMemory(addr: Literal, val: Literal): void { this._memory[addr] = val; }


  performExternalAction(action: () => void): void { action(); }


  isRunning(): boolean { return this._isRunning; }

  halt(): void { this._isRunning = false; }
}
