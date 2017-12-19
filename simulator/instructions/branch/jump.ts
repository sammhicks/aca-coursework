import { BranchInstruction } from "../instruction";
import { Literal, PC } from "../../components/basic-types";

export class Jump extends BranchInstruction {
  readonly i0: Literal;

  static readonly pneumonic: string = "j";

  get duration() { return 1; }

  getRequirements() { return []; }

  execute() { return []; }

  expectedPC(pc: PC): PC { return pc + this.i0; }
};
