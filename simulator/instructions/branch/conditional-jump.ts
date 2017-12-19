import { BranchInstruction } from "../instruction";
import { Literal, Register, PC } from "../../components/basic-types";
import { BranchPredictionError, RegisterReleaser } from "../../components/execution-result";
import { ReadsRegister } from "../../components/instruction-requirements";
import { HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class ConditionalJump extends BranchInstruction {
  readonly i0: Literal;
  readonly inv: boolean;
  readonly cond: Literal;
  readonly r1: Register;

  static readonly pneumonic: string = "cj";

  get duration() { return 2; }

  getRequirements(sync: RegisterSync) { return [new ReadsRegister(sync, this.r1)]; }

  execute(rf: HasRegisters, pc: PC) {
    const conditionMatchesVariable = this.cond == rf.getRegister(this.r1);
    const withInversion = conditionMatchesVariable != this.inv;

    const newPC = pc + (withInversion ? this.i0 : 0);

    return (newPC == this.expectedPC(pc)) ? [new RegisterReleaser(this.r1)] : [new RegisterReleaser(this.r1), new BranchPredictionError(newPC)];
  }

  expectedPC(pc: PC) {
    return this.i0 < 0 ? (pc + this.i0) : pc;
  }
};
