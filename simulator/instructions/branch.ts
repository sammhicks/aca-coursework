import { BranchInstruction } from "./instruction";
import { Literal } from "../components/basic-types";
import { RegisterWriter, PCWriter } from "../components/execution-result";
import { LR_INDEX, ReadableRegisterFile } from "../components/register-file";
import { BranchInteractions } from "../components/instruction-interactions";

export class Branch extends BranchInstruction {
  readonly i0: Literal;

  static readonly pneumonic: string = "b";

  get duration() { return 2; }

  get requirements() { return new BranchInteractions(false, []); }

  get effects() { return new BranchInteractions(true, [LR_INDEX]); }

  execute(rf: ReadableRegisterFile) {
    return [
      new PCWriter(this.i0),
      new RegisterWriter(LR_INDEX, rf.pc)
    ];
  }
};
