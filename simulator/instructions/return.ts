import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, PCWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Return extends Instruction {
  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions([], false, true); }

  effects(): InstructionInteractions { return new InstructionInteractions([], true); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new PCWriter(rf.lr)]; }
};
