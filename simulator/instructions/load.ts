import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Load extends Instruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  duration: number = 4;

  requirements(): InstructionInteractions { return new InstructionInteractions(this.r12, false, false, true); }

  effects(): InstructionInteractions { return new InstructionInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.memory[rf.lookupRegisters(this.r12).reduce((acc, item) => acc + item, this.i3)])
    ];
  }
};
