import { BranchInstruction } from "./instruction";
import { Literal } from "../components/basic-types";
import { PCWriter } from "../components/execution-result";
import { HasPC, HasRegisters } from "../components/register-file";
import { BranchInteractions } from "../components/instruction-interactions";

export class Jump extends BranchInstruction {
  readonly i0: Literal;

  static readonly pneumonic: string = "j";

  get duration() { return 1; }

  get requirements() { return new BranchInteractions(true, []); }

  get effects() { return new BranchInteractions(true, []); }

  execute(rf: HasPC & HasRegisters): [PCWriter] { return [new PCWriter(rf.pc + this.i0)]; }
};
