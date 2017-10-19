import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, PCWriter } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions, PCInteractions } from "../components/instruction-interactions";

export class ConditionalJump extends Instruction {
  private i0: Literal;
  private inv: boolean;
  private cond: Literal;
  private r1: Register;

  static pneumonic: string = "cj";

  duration(): number { return 2; }

  requirements(): InstructionInteractions { return new RegisterInteractions([this.r1]); }

  effects(): InstructionInteractions { return new PCInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    const conditionMatchesVariable = this.cond == rf.registers[this.r1];
    const withInversion = conditionMatchesVariable != this.inv;
    return withInversion ? [new PCWriter(rf.pc + this.i0)] : [];
  }
};
