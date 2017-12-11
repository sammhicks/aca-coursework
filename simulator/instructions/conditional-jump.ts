import { BranchInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { PCWriter, BranchPredictionError } from "../components/execution-result";
import { HasPC, HasRegisters } from "../components/register-file";
import { BranchInteractions } from "../components/instruction-interactions";

export class ConditionalJump extends BranchInstruction {
  readonly i0: Literal;
  readonly inv: boolean;
  readonly cond: Literal;
  readonly r1: Register;

  static readonly pneumonic: string = "cj";

  get duration() { return 2; }

  get requirements() { return new BranchInteractions(true, [this.r1]); }

  get effects() { return new BranchInteractions(true, []); }

  execute(rf: ReadableRegisterFile) {
    const conditionMatchesVariable = this.cond == rf.readRegister(this.r1);
    const withInversion = conditionMatchesVariable != this.inv;

    const pc = rf.pc + (withInversion ? this.i0 : 0);

    const writes = withInversion ? [new PCWriter(rf.pc + this.i0)] : [];

    return (pc == this.expectedPC(rf.pc)) ? writes : new BranchPredictionError(writes);
  }
};
