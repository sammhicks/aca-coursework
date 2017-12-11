import { Register, Literal } from "./basic-types";
import { HasPC, HasRegisters, HasMemory, ReadableRegisterFile, getRegisters } from "./register-file";
import { RegisterFileItemSync, PCSync, RegisterSync, MemorySync, RegisterFileSync } from "./register-file-sync";
import { areEqual } from "../util/compare";
import { Semaphore } from "../util/semaphore";

export interface InstructionRequirement {
  registerReads(rfs: RegisterFileSync): void;

  met(rf: ReadableRegisterFile, rfs: RegisterFileSync): boolean;
}

export class ReadsPC implements InstructionRequirement {
  constructor(readonly target: Semaphore) { }

  registerReads(rfs: PCSync) { rfs.getPCSync().readersCount += 1; }

  met(rf: HasPC, rfs: PCSync) { return areEqual(this.target, rfs.getPCSync().currentState); }
}

export class SetsPC extends ReadsPC {
  met(rf: HasPC, rfs: PCSync) { return super.met(rf, rfs) && rfs.getPCSync().readersCount == 0; }
}


export class ReadsRegister implements InstructionRequirement {
  constructor(readonly reg: Register, readonly target: Semaphore) { }

  registerReads(rfs: RegisterSync) { rfs.getRegisterSync(this.reg).readersCount += 1; }

  met(rf: HasRegisters, rfs: RegisterSync) { return areEqual(this.target, rfs.getRegisterSync(this.reg).currentState); }
}

export class SetsRegister extends ReadsRegister {
  met(rf: HasRegisters, rfs: RegisterSync) { return super.met(rf, rfs) && rfs.getRegisterSync(this.reg).readersCount == 0; }
}


export class ReadsMemory implements InstructionRequirement {
  constructor(readonly addrRegs: Register[], readonly addrOffset: Literal) { }

  getAddress(rf: HasRegisters) {
    return getRegisters(rf, this.addrRegs).reduce((acc, item) => acc + item, this.addrOffset);
  }

  met(rf: HasRegisters & HasMemory, rfs: MemorySync) {

  }
}
