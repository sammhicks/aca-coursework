import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, Halter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Halt extends Instruction {
  static pneumonic: string = "halt";

  duration: number = 1;

  halts: boolean = true;

  requirements(): InstructionInteractions { return new InstructionInteractions(); }

  effects(): InstructionInteractions { return new InstructionInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new Halter()]; }
};
