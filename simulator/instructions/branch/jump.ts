import { BranchInstruction } from "../instruction";
import { Literal, PC } from "../../components/basic-types";
import { RegisterWriter, PCWriter } from "../../components/execution-result";
import { ReadsPC, SetsPC, SetsRegister } from "../../components/instruction-requirements";
import { LR_INDEX, ReadableRegisterFile, HasPC } from "../../components/register-file";
import { PCSync, RegisterSync } from "../../components/register-file-sync";

export class Jump extends BranchInstruction {
  readonly i0: Literal;

  static readonly pneumonic: string = "j";

  get duration() { return 1; }

  getReadRequirements(sync: PCSync) { return [new ReadsPC(sync)]; }

  getWriteRequirements(sync: PCSync) { return [new SetsPC(sync)]; }

  execute(rf: HasPC): [PCWriter] { return [new PCWriter(rf.getPC() + this.i0)]; }

  expectedPC(pc: PC): PC { return pc + this.i0; }
};
