import { BranchInstruction } from "../instruction";
import { Literal, Register, PC } from "../../components/basic-types";
import { RegisterWriter, PCWriter, BranchPredictionError } from "../../components/execution-result";
import { ReadsPC, SetsPC, SetsRegister, ReadsRegister } from "../../components/instruction-requirements";
import { LR_INDEX, ReadableRegisterFile, HasPC, HasRegisters } from "../../components/register-file";
import { PCSync, RegisterSync } from "../../components/register-file-sync";

export class ConditionalJump extends BranchInstruction {
  readonly i0: Literal;
  readonly inv: boolean;
  readonly cond: Literal;
  readonly r1: Register;

  static readonly pneumonic: string = "cj";

  get duration() { return 2; }

  getReadRequirements(sync: PCSync & RegisterSync) { return [new ReadsPC(sync), new ReadsRegister(sync, this.r1)]; }

  getWriteRequirements(sync: PCSync) { return [new SetsPC(sync)]; }

  execute(rf: HasPC & HasRegisters) {
    const conditionMatchesVariable = this.cond == rf.getRegister(this.r1);
    const withInversion = conditionMatchesVariable != this.inv;

    const pc = rf.getPC() + (withInversion ? this.i0 : 0);

    return (pc == this.expectedPC(rf.getPC())) ? [new PCWriter(pc)] : new BranchPredictionError(pc);
  }
};
