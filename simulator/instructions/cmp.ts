import { Instruction } from "./instruction";
import { Register } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { RegisterRequirements } from "../components/register-requirements";
import { compare } from "../util/compare";

export class Cmp extends Instruction {
  private r0: Register;
  private r1: Register;
  private r2: Register;

  requirements(): RegisterRequirements { return new RegisterRequirements([this.r1, this.r2]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.registers[this.r1], rf.registers[this.r2])
    ];
  }
};
