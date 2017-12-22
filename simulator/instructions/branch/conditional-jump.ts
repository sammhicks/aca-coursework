import { BranchInstruction } from "../instruction";
import { Literal, Register, PC } from "../../components/basic-types";
import { BranchPredictionSuccess, BranchPredictionError, RegisterReleaser, TookBranch } from "../../components/execution-result";
import { ReadsRegister } from "../../components/instruction-requirements";
import { HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";
import { Prediction } from "../../components/prediction";

enum BranchPredictionTypes {
  None,
  Static,
  Dynamic
};

const branchPredictionType: BranchPredictionTypes = BranchPredictionTypes.Dynamic;

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

    const newPC = pc + 1 + (branchTaken ? this.i0 : 0);

    return [
      new RegisterReleaser(this.r1),
      new TookBranch(pc, branchTaken),
      ((branchPredictionType != BranchPredictionTypes.None) && (newPC == expectedPC)) ? new BranchPredictionSuccess() : new BranchPredictionError(newPC)
    ];
  }

  expectedPC(pc: PC, prediction: Prediction) {
    const staticGuess = this.i0 < 0;
    const dynamicGuess = prediction.branchPrediction.lookupValue(pc, staticGuess);

    const takeBranch = (branchPredictionType == BranchPredictionTypes.Static) ? staticGuess : dynamicGuess;

    return pc + 1 + (takeBranch ? this.i0 : 0);
  }
};
