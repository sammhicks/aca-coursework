import { Instruction } from "./instruction";
import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, PCWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Jump extends Instruction {
  private i0: Literal;

  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions([], true); }

  effects(): InstructionInteractions { return new InstructionInteractions([], true); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new PCWriter(rf.pc + this.i0)]; }
};
