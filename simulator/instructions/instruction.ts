import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter } from "../components/register-file";
import { InstructionRequirements } from "../components/instruction-requirements";

export abstract class Instruction {
  public name: string;

  abstract duration: number;

  abstract requirements(): InstructionRequirements

  abstract execute(rf: RegisterFile): RegisterFileWriter[];

  expectedPC(pc: number): Literal { return pc + 1; }
};
