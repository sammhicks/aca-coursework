import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export abstract class Instruction {
  public name: string;

  abstract duration: number;

  abstract requirements(): InstructionInteractions;

  abstract effects(): InstructionInteractions;

  abstract execute(rf: RegisterFile): RegisterFileWriter[];

  expectedPC(pc: number): Literal { return pc + 1; }
};
