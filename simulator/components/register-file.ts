import { Register, Literal } from "./register";

export const LR_INDEX = 0;

export class RegisterFile {
  public pc: Literal;
  public registers: Literal[];
  public memory: Literal[];
  public running: boolean;

  constructor() {
    this.pc = 0;
    this.registers = [];
    this.memory = [];
    this.running = true;
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

export class MemoryWriter implements RegisterFileWriter {
  constructor(private addr: Literal, private v: Literal) { }

  write(rf: RegisterFile): void { rf.memory[this.addr] = this.v; }
}

export class ExternalAction implements RegisterFileWriter {
  constructor(private action: (rf: RegisterFile) => void) { }

  write(rf: RegisterFile): void { this.action(rf); }
}

export class Halter implements RegisterFileWriter {
  write(rf: RegisterFile): void { rf.running = false; }
}
