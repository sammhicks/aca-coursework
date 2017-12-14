import { BranchInstruction } from "../instruction";
import { Literal, PC } from "../../components/basic-types";
import { RegisterWriter, PCWriter, BranchPredictionError } from "../../components/execution-result";
import { ReadsPC, SetsPC, SetsRegister, ReadsRegister } from "../../components/instruction-requirements";
import { LR_INDEX, HasPC, HasRegisters } from "../../components/register-file";
import { PCSync, RegisterSync } from "../../components/register-file-sync";

export class Return extends BranchInstruction {
  static readonly pneumonic: string = "ret";

  get duration() { return 1; }

  getReadRequirements(sync: RegisterSync) { return [new ReadsRegister(sync, LR_INDEX)]; }

  getWriteRequirements(sync: PCSync) { return [new SetsPC(sync)]; }

  execute(rf: HasRegisters) { return new BranchPredictionError(rf.getRegister(LR_INDEX)); }
};
