import { Literal, RegisterFile, RegisterFileWriter, RegisterRequirements } from "../components";

export abstract class Instruction {
  abstract requirements(): RegisterRequirements

  abstract execute(rf: RegisterFile): RegisterFileWriter[];

  expectedPC(pc: number): Literal { return pc + 1; }
};
