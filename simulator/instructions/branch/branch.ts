import { BranchInstruction } from "../instruction";
import { Literal, PC } from "../../components/basic-types";
import { RegisterWriter } from "../../components/execution-result";
import { SetsRegister } from "../../components/instruction-requirements";
import { LR_INDEX } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Branch extends BranchInstruction {
  readonly i0: Literal;

  static readonly pneumonic: string = "b";

  get duration() { return 2; }

  getReadRequirements() { return []; }

  getWriteRequirements(sync: RegisterSync) { return [new SetsRegister(sync, LR_INDEX)]; }

  execute(rf: never, pc: PC) {
    return [
      new RegisterWriter(LR_INDEX, pc)
    ];
  }

  expectedPC(pc: PC): PC { return this.i0; }
};
