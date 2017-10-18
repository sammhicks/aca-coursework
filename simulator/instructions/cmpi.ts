import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { RegisterRequirements } from "../components/register-requirements";
import { compare } from "../util/compare";

export class CmpI extends Instruction {
  private r0: Register;
  private r1: Register;
  private i2: Literal;

  requirements(): RegisterRequirements { return new RegisterRequirements([this.r1]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.registers[this.r1], this.i2)
    ];
  }
};
