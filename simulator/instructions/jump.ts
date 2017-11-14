import { BranchInstruction } from "./instruction";
import { Literal } from "../components/basic-types";
import { ExecutionResult, PCWriter } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, NoInteractions, PCInteractions } from "../components/instruction-interactions";

export class Jump extends BranchInstruction {
  readonly i0: Literal;

  static readonly pneumonic: string = "j";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new PCInteractions(); }

  execute(rf: LikeRegisterFile): ExecutionResult[] { return [new PCWriter(rf.pc + this.i0)]; }
};
