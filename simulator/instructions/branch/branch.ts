import { BranchInstruction } from "../instruction";
import { Literal, PC } from "../../components/basic-types";
import { RegisterWriter, PCWriter } from "../../components/execution-result";
import { ReadsPC, SetsPC, SetsRegister } from "../../components/instruction-requirements";
import { LR_INDEX, HasPC } from "../../components/register-file";
import { PCSync, RegisterSync } from "../../components/register-file-sync";

export class Branch extends BranchInstruction {
  readonly i0: Literal;

  static readonly pneumonic: string = "b";

  get duration() { return 2; }

  getReadRequirements(sync: PCSync) { return [new ReadsPC(sync)]; }

  getWriteRequirements(sync: PCSync & RegisterSync) { return [new SetsPC(sync), new SetsRegister(sync, LR_INDEX)]; }

  execute(rf: HasPC) {
    return [
      new PCWriter(this.i0),
      new RegisterWriter(LR_INDEX, rf.getPC())
    ];
  }

  expectedPC(pc: PC): PC { return this.i0; }
};
