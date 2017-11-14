import { MemoryInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { ExecutionResult, RegisterWriter } from "../components/execution-result";
import { LikeRegisterFile, lookupRegisters } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions, MemoryInteractions } from "../components/instruction-interactions";

export class Load extends MemoryInstruction {
  readonly r0: Register;
  readonly r12: Register[];
  readonly i3: Literal

  static readonly pneumonic: string = "ld";

  get duration(): number { return 2; }

  get requirements(): InstructionInteractions { return new MemoryInteractions(this.r12); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.readMemory(lookupRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3)))
    ];
  }
};
