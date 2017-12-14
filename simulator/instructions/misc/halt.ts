import { MiscInstruction } from "../instruction";
import { Halter } from "../../components/execution-result";
import { PC } from "../../components/basic-types";

export class Halt extends MiscInstruction {
  static readonly pneumonic: string = "halt";

  get duration() { return 1; }

  getReadRequirements() { return [] as never[]; }

  getWriteRequirements() { return [] as never[]; }

  execute() { return [new Halter()]; }

  get isNonSequential() { return true; }

  expectedPC(pc: number): PC { return pc; }
};
