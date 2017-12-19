import { PC, Literal, Register } from "./basic-types";
import { HasWritableRegisters, HasWritableMemory, PerformsExternalActions, Halts, WritableRegisterFile, HandlesBranchPredictionError } from "./register-file";

export interface ExecutionResult {
  consume(rf: WritableRegisterFile): void;
}

export class NoOpResult {
  consume() { }
}

export class RegisterWriter implements ExecutionResult {
  constructor(readonly reg: Register, readonly val: Literal) { }

  consume(rf: HasWritableRegisters) { rf.updateRegister(this.reg, this.val) }
}

export class RegisterReleaser implements ExecutionResult {
  constructor(readonly reg: Register) { }

  consume(rf: HasWritableRegisters) { rf.releaseRegister(this.reg) }
}

export function registerReleasers(dst: Register | null, src: (Register | null)[]): RegisterReleaser[] {
  return (src.filter(r => (r != null && r != undefined && r != dst)) as Register[]).map(r => new RegisterReleaser(r));
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

export class BranchPredictionError implements ExecutionResult {
  constructor(readonly pc: PC) { }

  consume(rf: HandlesBranchPredictionError) { rf.handleBranchPredictionError(this.pc); }
}


export interface ExecutionResultsHandler {
  handleExecutionResults(results: ExecutionResult[]): void;
}
