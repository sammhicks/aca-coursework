import { BranchInstruction } from "./instruction";
import { Literal } from "../components/basic-types";
import { ExecutionResult, RegisterWriter, PCWriter } from "../components/execution-result";
import { LR_INDEX, LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, NoInteractions, PCInteractions } from "../components/instruction-interactions";

export class Branch extends BranchInstruction {
  readonly i0: Literal;

  static readonly pneumonic: string = "b";

  get duration(): number { return 2; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new PCInteractions([LR_INDEX]); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    return [
      new RegisterWriter(LR_INDEX, rf.pc),
      new PCWriter(this.i0)
    ];
  }
};
