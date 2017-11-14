import { IOInstruction } from "./instruction";
import { ExecutionResult, ExternalAction } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, NoInteractions } from "../components/instruction-interactions";

export class Log extends IOInstruction {
  readonly message: string;

  static readonly pneumonic: string = "log";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new NoInteractions(); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    const self = this;
    return [new ExternalAction(() => console.log("Log: \"%s\"", self.message))];
  }
};
