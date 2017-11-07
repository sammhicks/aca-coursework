import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, Halter } from "../components/register-file";
import { InstructionInteractions, NoInteractions } from "../components/instruction-interactions";

export class Halt extends Instruction {
  static pneumonic: string = "halt";

  get duration(): number { return 1; }

  get halts(): boolean { return true; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new NoInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new Halter()]; }
};
