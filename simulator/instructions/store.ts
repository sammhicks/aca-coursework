import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, MemoryWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Store extends Instruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  static pneumonic: string = "st";

  duration: number = 4;

  requirements(): InstructionInteractions { return new InstructionInteractions(this.r12, false, false, true); }

  effects(): InstructionInteractions { return new InstructionInteractions([], false, false, true); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new MemoryWriter(
        rf.lookupRegisters(this.r12).reduce((acc, item) => acc + item, this.i3),
        rf.registers[this.r0])
    ];
  }
};
