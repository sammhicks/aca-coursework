import { IOInstruction } from "../instruction";
import { ExternalAction } from "../../components/execution-result";

export class Log extends IOInstruction {
  readonly message: string;

  static readonly pneumonic: string = "log";

  get duration() { return 1; }

  getReadRequirements() { return [] as never[]; }

  getWriteRequirements() { return [] as never[]; }

  execute() {
    const self = this;
    return [new ExternalAction(() => console.log("Log: \"%s\"", self.message))];
  }
};
