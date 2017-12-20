import { BranchInstruction } from "../instruction";
import { PC } from "../../components/basic-types";
import { RegisterReleaser, Returned, ReturnPredictionSuccess, ReturnPredictionError } from "../../components/execution-result";
import { ReadsRegister } from "../../components/instruction-requirements";
import { LR_INDEX, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";
import { Prediction } from "../../components/prediction";

export class Return extends BranchInstruction {
  static readonly pneumonic: string = "ret";

  get duration() { return 1; }

  getRequirements(sync: RegisterSync) { return [new ReadsRegister(sync, LR_INDEX)]; }

  execute(rf: HasRegisters, pc: PC, expectedPC: PC) {
    const newPC: PC = rf.getRegister(LR_INDEX);

    return [
      new RegisterReleaser(LR_INDEX),
      new Returned(pc, newPC),
      newPC == expectedPC ? new ReturnPredictionSuccess() : new ReturnPredictionError(newPC)
    ];
  }

  expectedPC(pc: PC, prediction: Prediction) { return prediction.returnPrediction.lookupValue(pc, pc); }
};
