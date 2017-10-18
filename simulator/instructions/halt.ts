import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, Halter } from "../components/register-file";
import { InstructionInteractions, NoInteractions } from "../components/instruction-interactions";

export class Halt extends Instruction {
  static pneumonic: string = "halt";

  duration: number = 1;

  halts: boolean = true;

  requirements(): InstructionInteractions { return new NoInteractions(); }

  effects(): InstructionInteractions { return new NoInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new Halter()]; }
};
