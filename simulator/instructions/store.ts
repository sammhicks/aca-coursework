import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, MemoryWriter } from "../components/register-file";
import { InstructionInteractions, MemoryInteractions, RegisterInteractions } from "../components/instruction-interactions";

export class Store extends Instruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  static pneumonic: string = "st";

  get duration(): number { return 2; }

  get requirements(): InstructionInteractions { return new MemoryInteractions(this.r12); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new MemoryWriter(
        rf.lookupRegisters(this.r12).reduce((acc, item) => acc + item, this.i3),
        rf.registers[this.r0])
    ];
  }
};
