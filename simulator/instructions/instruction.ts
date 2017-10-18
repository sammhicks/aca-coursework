import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter } from "../components/register-file";
import { RegisterRequirements } from "../components/register-requirements";

export abstract class Instruction {
  public name: string;

  abstract requirements(): RegisterRequirements

  abstract execute(rf: RegisterFile): RegisterFileWriter[];

  expectedPC(pc: number): Literal { return pc + 1; }
};
