import { MiscInstruction } from "../instruction";
import { Halter } from "../../components/execution-result";
import { PC } from "../../components/basic-types";

export class Halt extends MiscInstruction {
  static readonly pneumonic: string = "halt";

  get duration() { return 1; }

  getRequirements() { return []; }

  execute() { return [new Halter()]; }

  get isNonSequential() { return true; }

  get halts() { return true; }

  expectedPC(pc: number): PC { return pc; }
};
