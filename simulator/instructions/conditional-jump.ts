import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, PCWriter } from "../components/register-file";
import { InstructionRequirements } from "../components/instruction-requirements";

export class ConditionalJump extends Instruction {
  private i0: Literal;
  private inv: boolean;
  private cond: Literal;
  private r1: Register;

  duration: number = 1;

  requirements(): InstructionRequirements { return new InstructionRequirements([], true); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return (this.inv == (this.cond == rf.registers[this.r1]) ? [] : [new PCWriter(rf.pc + this.i0)]);
  }
};
