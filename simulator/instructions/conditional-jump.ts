import { Instruction, BranchPredictionError } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, PCWriter } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions, PCInteractions } from "../components/instruction-interactions";

export class ConditionalJump extends Instruction {
  private i0: Literal;
  private inv: boolean;
  private cond: Literal;
  private r1: Register;

  static pneumonic: string = "cj";

  get duration(): number { return 2; }

  get requirements(): InstructionInteractions { return new RegisterInteractions([this.r1]); }

  get effects(): InstructionInteractions { return new PCInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] | BranchPredictionError {
    const conditionMatchesVariable = this.cond == rf.registers[this.r1];
    const withInversion = conditionMatchesVariable != this.inv;

    const pc = rf.pc + (withInversion ? this.i0 : 0);

    const writes = withInversion ? [new PCWriter(rf.pc + this.i0)] : [];

    return (pc == this.expectedPC(rf.pc)) ? writes : new BranchPredictionError(writes);
  }
};
