import { MemoryInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { MemoryWriter } from "../../components/execution-result";
import { ReadsRegister, WritesToMemory } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters, HasMemory } from "../../components/register-file";
import { RegisterSync, MemorySync } from "../../components/register-file-sync";

export class Store extends MemoryInstruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  static readonly pneumonic: string = "st";

  get duration() { return 2; }

  getReadRequirements(sync: RegisterSync & MemorySync, rf: HasRegisters & HasMemory) { return this.r12.map(r => new ReadsRegister(sync, r)); }

  getWriteRequirements(sync: RegisterSync & MemorySync, rf: HasRegisters & HasMemory) { return [new WritesToMemory(sync, rf, this.r12, this.i3)]; }

  execute(rf: HasRegisters & HasMemory): [MemoryWriter] {
    return [
      new MemoryWriter(
        getRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3),
        rf.getRegister(this.r0))
    ];
  }
};
