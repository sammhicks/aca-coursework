import { MiscInstruction } from "./instruction";
import { Halter } from "../components/execution-result";
import { Halts } from "../components/register-file";
import { MiscInteractions } from "../components/instruction-interactions";

export class Halt extends MiscInstruction {
  static readonly pneumonic: string = "halt";

  get duration() { return 1; }

  get halts() { return true; }

  get requirements() { return new MiscInteractions(); }

  get effects() { return new MiscInteractions(); }

  execute(rf: Halts): [Halter] { return [new Halter()]; }
};
