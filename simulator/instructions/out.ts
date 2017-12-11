import { IOInstruction } from "./instruction";
import { Register } from "../components/basic-types";
import { ExternalAction } from "../components/execution-result";
import { HasRegisters } from "../components/register-file";
import { IOInteractions } from "../components/instruction-interactions";

export class Out extends IOInstruction {
  readonly label: string;
  readonly r0: Register;

  static readonly pneumonic: string = "out";

  get duration() { return 1; }

  get requirements() { return new IOInteractions([this.r0]); }

  get effects() { return new IOInteractions([]); }

  execute(rf: ReadableRegisterFile) {
    const self = this;
    return [new ExternalAction(() => console.log("Out: %s (%d) = %d", self.label, self.r0, rf.readRegister(self.r0)))];
  }
};
