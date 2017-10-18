import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, Halter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class NoOp extends Instruction {
  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions(); }

  effects(): InstructionInteractions { return new InstructionInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return []; }
};
