import { IOInstruction } from "../instruction";
import { ExternalAction } from "../../components/execution-result";

export class Log extends IOInstruction {
  readonly message: string;

  static readonly pneumonic: string = "log";

  get duration() { return 1; }

  getRequirements() { return []; }

  execute() {
    const self = this;
    return [new ExternalAction(() => console.log("Log: \"%s\"", self.message))];
  }
};
