import { MiscInstruction } from "./instruction";
import { ExecutionResult } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, NoInteractions } from "../components/instruction-interactions";

export class NoOp extends MiscInstruction {
  static readonly pneumonic: string = "noop";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new NoInteractions(); }

  execute(rf: LikeRegisterFile): ExecutionResult[] { return []; }
};
