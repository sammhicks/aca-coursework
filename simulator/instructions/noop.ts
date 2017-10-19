import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter } from "../components/register-file";
import { InstructionInteractions, NoInteractions } from "../components/instruction-interactions";

export class NoOp extends Instruction {
  static pneumonic: string = "noop";

  duration(): number { return 1; }

  requirements(): InstructionInteractions { return new NoInteractions(); }

  effects(): InstructionInteractions { return new NoInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return []; }
};
