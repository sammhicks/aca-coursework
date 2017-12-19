import { BranchInstruction } from "../instruction";
import { PC } from "../../components/basic-types";
import { BranchPredictionError, RegisterReleaser } from "../../components/execution-result";
import { ReadsRegister } from "../../components/instruction-requirements";
import { LR_INDEX, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Return extends BranchInstruction {
  static readonly pneumonic: string = "ret";

  get duration() { return 1; }

  getRequirements(sync: RegisterSync) { return [new ReadsRegister(sync, LR_INDEX)]; }

  execute(rf: HasRegisters) { return [new RegisterReleaser(LR_INDEX), new BranchPredictionError(rf.getRegister(LR_INDEX))]; }

  expectedPC(pc: PC) { return pc; }
};
