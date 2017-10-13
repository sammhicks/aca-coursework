import { Register, Literal } from "./register";

export class RegisterFile {
  public pc: Literal;
  public lr: Literal;
  public registers: Literal[];

  constructor(registerCount: number) {
    this.pc = 0;
    this.lr = 0;
    this.registers = Array<Literal>(registerCount).fill(0)
  }

  lookupRegisters(rs: Register[]): Literal[] { return rs.map(r => this.registers[r]); }
}

export interface RegisterFileWriter {
  write(rf: RegisterFile): void;
}

export class RegisterWriter implements RegisterFileWriter {
  constructor(private r: Register, private v: Literal) { }

  write(rf: RegisterFile): void { rf.registers[this.r] = this.v; }
}

export class PCWriter implements RegisterFileWriter {
  constructor(private v: Literal) { }

  write(rf: RegisterFile): void { rf.pc = this.v; }
}

export class LRWriter implements RegisterFileWriter {
  constructor(private v: Literal) { }

  write(rf: RegisterFile): void { rf.lr = this.v; }
}
