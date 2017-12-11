import { Literal, Register } from "./basic-types";
import { HasWritablePC, HasWritableRegisters, HasWritableMemory, PerformsExternalActions, Halts, WritableRegisterFile } from "./register-file";

export interface ExecutionResult {
  consume(rf: WritableRegisterFile): void;
}

export class PCWriter implements ExecutionResult {
  constructor(readonly val: Literal) { }

  consume(rf: HasWritablePC) { rf.updatePC(this.val); }
}

export class PCReleaser implements ExecutionResult {
  consume(rf: HasWritablePC) { rf.releasePC(); }
}

export class RegisterWriter implements ExecutionResult {
  constructor(readonly reg: Register, readonly val: Literal) { }

  consume(rf: HasWritableRegisters) { rf.updateRegister(this.reg, this.val) }
}

export class RegisterReleaser implements ExecutionResult {
  constructor(readonly reg: Register) { }

  consume(rf: HasWritableRegisters) { rf.releaseRegister(this.reg) }
}

export class MemoryWriter implements ExecutionResult {
  constructor(readonly addr: Literal, readonly val: Literal) { }

  consume(rf: HasWritableMemory) { rf.writeMemory(this.addr, this.val); }
}

export class MemoryReleaser implements ExecutionResult {
  constructor(readonly addr: Literal) { }

  consume(rf: HasWritableMemory) { rf.releaseMemory(this.addr); }
}

export class ExternalAction implements ExecutionResult {
  constructor(readonly action: () => void) { }

  consume(rf: PerformsExternalActions) { rf.performExternalAction(this.action); }
}

export class Halter implements ExecutionResult {
  consume(rf: Halts) { rf.halt(); }
}

export class BranchPredictionError {
  constructor(public writes: ExecutionResult[]) { };
};
