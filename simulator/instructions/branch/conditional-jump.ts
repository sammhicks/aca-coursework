import { BranchInstruction } from "../instruction";
import { Literal, Register, PC } from "../../components/basic-types";
import { BranchPredictionError, RegisterReleaser, TookBranch } from "../../components/execution-result";
import { ReadsRegister } from "../../components/instruction-requirements";
import { HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";
import { Prediction } from "../../components/prediction";

export class ConditionalJump extends BranchInstruction {
  readonly i0: Literal;
  readonly inv: boolean;
  readonly cond: Literal;
  readonly r1: Register;

  static readonly pneumonic: string = "cj";

  get duration() { return 2; }

  getRequirements(sync: RegisterSync) { return [new ReadsRegister(sync, this.r1)]; }

  execute(rf: HasRegisters, pc: PC, expectedPC: PC) {
    const conditionMatchesVariable = this.cond == rf.getRegister(this.r1);
    const branchTaken = conditionMatchesVariable != this.inv;

    const newPC = pc + (branchTaken ? this.i0 : 0);

    return ([] as (RegisterReleaser | BranchPredictionError | TookBranch)[])
      .concat([new RegisterReleaser(this.r1), new TookBranch(pc, branchTaken)])
      .concat(newPC == expectedPC ? [] : new BranchPredictionError(newPC));
  }

  expectedPC(pc: PC, prediction: Prediction) {
    const takeBranch = prediction.branchPrediction.lookupValue(pc, this.i0 < 0);

    return takeBranch ? (pc + this.i0) : pc;
  }
};
