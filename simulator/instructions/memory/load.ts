import { MemoryInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { RegisterWriter } from "../../components/execution-result";
import { ReadsRegister, SetsRegister, ReadsFromMemory, WritesToMemory } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters, HasMemory } from "../../components/register-file";
import { RegisterSync, MemorySync } from "../../components/register-file-sync";

export class Load extends MemoryInstruction {
  readonly r0: Register;
  readonly r12: Register[];
  readonly i3: Literal

  static readonly pneumonic: string = "ld";

  get duration() { return 2; }

  getReadRequirements(sync: RegisterSync & MemorySync, rf: HasRegisters & HasMemory) {
    return (this.r12.map(r => new ReadsRegister(sync, r)) as (ReadsRegister | ReadsFromMemory)[]).concat([new ReadsFromMemory(sync, rf, this.r12, this.i3)]);
  }

  getWriteRequirements(sync: RegisterSync) { return [new SetsRegister(sync, this.r0)]; }

  execute(rf: HasRegisters & HasMemory): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        rf.readMemory(getRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3)))
    ];
  }
};
