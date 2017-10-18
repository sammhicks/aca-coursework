import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions } from "../components/instruction-interactions";
import { compare } from "../util/compare";

export class CompareImmediate extends Instruction {
  private r0: Register;
  private r1: Register;
  private i2: Literal;

  static pneumonic: string = "cmpi";

  duration: number = 1;

  requirements(): InstructionInteractions { return new RegisterInteractions([this.r1]); }

  effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.registers[this.r1], this.i2))
    ];
  }
};
