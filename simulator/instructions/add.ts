import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Add extends Instruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal;

  static pneumonic: string = "add";

  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions(this.r12); }

  effects(): InstructionInteractions { return new InstructionInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.lookupRegisters(this.r12).reduce((acc, item) => acc + item, this.i3))
    ];
  }
};
