import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class NoOp extends Instruction {
  static pneumonic: string = "noop";

  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions(); }

  effects(): InstructionInteractions { return new InstructionInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return []; }
};
