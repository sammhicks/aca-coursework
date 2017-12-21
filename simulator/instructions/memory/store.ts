import { MemoryInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { MemoryWriter, RegisterReleaser } from "../../components/execution-result";
import { ReadsRegister, WritesToMemory } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters, HasMemory } from "../../components/register-file";
import { RegisterSync, MemorySync } from "../../components/register-file-sync";

export class Store extends MemoryInstruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  static readonly pneumonic: string = "st";

  get duration() { return 2; }

  getRequirements(sync: RegisterSync & MemorySync, rf: HasRegisters & HasMemory) {
    return ([] as (ReadsRegister | WritesToMemory)[])
      .concat((this.r0 in this.r12) ? [] : [new ReadsRegister(sync, this.r0)])
      .concat([new WritesToMemory(sync, rf, null, this.r12, this.i3)]);
  }

  execute(rf: HasRegisters & HasMemory) {
    const addr = getRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3);

    return ([] as (RegisterReleaser | MemoryWriter)[])
      .concat((this.r0 in this.r12) ? [] : [new RegisterReleaser(this.r0)])
      .concat(this.r12.map(r => new RegisterReleaser(r)))
      .concat([new MemoryWriter(addr, rf.getRegister(this.r0))]);
  }
};
