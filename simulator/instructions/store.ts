import { MemoryInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { MemoryWriter } from "../components/execution-result";
import { HasRegisters, HasMemory, lookupRegisters } from "../components/register-file";
import { MemoryInteractions } from "../components/instruction-interactions";

export class Store extends MemoryInstruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  static readonly pneumonic: string = "st";

  get duration() { return 2; }

  get requirements() { return new MemoryInteractions([this.r0].concat(this.r12), false); }

  get effects() { return new MemoryInteractions([], true); }

  execute(rf: HasRegisters & HasMemory): [MemoryWriter] {
    return [
      new MemoryWriter(
        lookupRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3),
        rf.readRegister(this.r0))
    ];
  }
};
