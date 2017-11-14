import { MemoryInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { ExecutionResult, MemoryWriter } from "../components/execution-result";
import { LikeRegisterFile, lookupRegisters } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions, MemoryInteractions } from "../components/instruction-interactions";

export class Store extends MemoryInstruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  static readonly pneumonic: string = "st";

  get duration(): number { return 2; }

  get requirements(): InstructionInteractions { return new MemoryInteractions(this.r12); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    return [
      new MemoryWriter(
        lookupRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3),
        rf.readRegister(this.r0))
    ];
  }
};
