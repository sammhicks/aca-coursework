import { MemoryInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { RegisterWriter, RegisterReleaser, MemoryReleaser, registerReleasers } from "../../components/execution-result";
import { SetsRegister, ReadsFromMemory } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters, HasMemory } from "../../components/register-file";
import { RegisterSync, MemorySync } from "../../components/register-file-sync";

export class Load extends MemoryInstruction {
  readonly r0: Register;
  readonly r12: Register[];
  readonly i3: Literal

  static readonly pneumonic: string = "ld";

  get duration() { return 2; }

  getRequirements(sync: RegisterSync & MemorySync, rf: HasRegisters & HasMemory) {
    return [new SetsRegister(sync, this.r0), new ReadsFromMemory(sync, rf, this.r0, this.r12, this.i3)];
  }

  execute(rf: HasRegisters & HasMemory) {
    const addr = getRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3);

    return ([] as (RegisterWriter | RegisterReleaser | MemoryReleaser)[])
      .concat([new RegisterWriter(this.r0, rf.readMemory(addr))])
      .concat(registerReleasers(this.r0, this.r12))
      .concat(new MemoryReleaser(addr));
  }
};
