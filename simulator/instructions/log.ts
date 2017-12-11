import { IOInstruction } from "./instruction";
import { ExternalAction } from "../components/execution-result";
import { HasRegisters } from "../components/register-file";
import { IOInteractions } from "../components/instruction-interactions";

export class Log extends IOInstruction {
  readonly message: string;

  static readonly pneumonic: string = "log";

  get duration() { return 1; }

  get requirements() { return new IOInteractions([]); }

  get effects() { return new IOInteractions([]); }

  execute(rf: ReadableRegisterFile) {
    const self = this;
    return [new ExternalAction(() => console.log("Log: \"%s\"", self.message))];
  }
};
