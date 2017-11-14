import { Literal, Register } from "./basic-types";
import { LikeRegisterFile } from "./register-file";

export interface ExecutionResult {
  consume(rf: LikeRegisterFile): void;
}

export class PCWriter implements ExecutionResult {
  constructor(readonly val: Literal) { }

  consume(rf: LikeRegisterFile) { rf.pc = this.val; }
}

export class RegisterWriter implements ExecutionResult {
  constructor(readonly reg: Register, readonly val: Literal) { }

  consume(rf: LikeRegisterFile) { rf.writeRegister(this.reg, this.val) }
}

export class MemoryWriter implements ExecutionResult {
  constructor(readonly addr: Literal, readonly val: Literal) { }

  consume(rf: LikeRegisterFile) { rf.writeMemory(this.addr, this.val); }
}

export class ExternalAction implements ExecutionResult {
  constructor(readonly action: () => void) { }

  consume(rf: LikeRegisterFile) { rf.performExternalAction(this.action); }
}

export class Halter implements ExecutionResult {
  consume(rf: LikeRegisterFile) { rf.halt(); }
}

export class BranchPredictionError {
  constructor(public writes: ExecutionResult[]) { };
};
