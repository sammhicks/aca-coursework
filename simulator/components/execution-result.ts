import { Literal, Register } from "./basic-types";
import { HasRegisterFileComponents, HasPC, HasRegisters, HasMemory, PerformsExternalInteractions, Halts } from "./register-file";

export interface ExecutionResult {
  consume(rf: HasRegisterFileComponents): void;
}

export class PCWriter implements ExecutionResult {
  constructor(readonly val: Literal) { }

  consume(rf: HasPC) { rf.pc = this.val; }
}

export class RegisterWriter implements ExecutionResult {
  constructor(readonly reg: Register, readonly val: Literal) { }

  consume(rf: HasRegisters) { rf.writeRegister(this.reg, this.val) }
}

export class MemoryWriter implements ExecutionResult {
  constructor(readonly addr: Literal, readonly val: Literal) { }

  consume(rf: HasMemory) { rf.writeMemory(this.addr, this.val); }
}

export class ExternalAction implements ExecutionResult {
  constructor(readonly action: () => void) { }

  consume(rf: PerformsExternalInteractions) { rf.performExternalAction(this.action); }
}

export class Halter implements ExecutionResult {
  consume(rf: Halts) { rf.halt(); }
}

export class BranchPredictionError {
  constructor(public writes: ExecutionResult[]) { };
};
