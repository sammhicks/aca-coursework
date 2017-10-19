import { Instruction } from "./instruction";
import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, PCWriter } from "../components/register-file";
import { InstructionInteractions, NoInteractions, PCInteractions } from "../components/instruction-interactions";

export class Jump extends Instruction {
  private i0: Literal;

  static pneumonic: string = "j";

  duration(): number { return 1; }

  requirements(): InstructionInteractions { return new NoInteractions(); }

  effects(): InstructionInteractions { return new PCInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new PCWriter(rf.pc + this.i0)]; }
};
