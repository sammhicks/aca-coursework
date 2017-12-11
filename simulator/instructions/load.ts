import { MemoryInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { RegisterWriter } from "../components/execution-result";
import { HasRegisters, HasMemory, getRegisters } from "../components/register-file";
import { MemoryInteractions } from "../components/instruction-interactions";

export class Load extends MemoryInstruction {
  readonly r0: Register;
  readonly r12: Register[];
  readonly i3: Literal

  static readonly pneumonic: string = "ld";

  get duration() { return 2; }

  get requirements() { return new MemoryInteractions(this.r12, true); }

  get effects() { return new MemoryInteractions([this.r0], false); }

  execute(rf: HasRegisters & HasMemory): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        rf.readMemory(getRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3)))
    ];
  }
};
