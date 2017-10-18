import { Instruction } from "./instruction";
import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, LRWriter, PCWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Branch extends Instruction {
  private i0: Literal;

  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions([], true, true); }

  effects(): InstructionInteractions { return new InstructionInteractions([], true, true); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new LRWriter(rf.pc + 1),
      new PCWriter(this.i0)
    ];
  }
};
