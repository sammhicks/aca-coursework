import { MiscInstruction } from "../instruction";
import { Halter } from "../../components/execution-result";
import { HasRegisterFileComponents } from "../../components/register-file";
import { PC } from "../../components/basic-types";

export class Halt extends MiscInstruction {
  static readonly pneumonic: string = "halt";

  get duration() { return 1; }

  getReadRequirements() { return [] as never[]; }

  getWriteRequirements() { return [] as never[]; }

  execute(rf: HasRegisterFileComponents): [Halter] { return [new Halter()]; }

  expectedPC(pc: number): PC { return pc; }
};
