import { BranchInstruction } from "./instruction";
import { ExecutionResult, PCWriter } from "../components/execution-result";
import { LikeRegisterFile, LR_INDEX } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions, PCInteractions } from "../components/instruction-interactions";

export class Return extends BranchInstruction {
  static readonly pneumonic: string = "ret";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new RegisterInteractions([LR_INDEX]); }

  get effects(): InstructionInteractions { return new PCInteractions(); }

  execute(rf: LikeRegisterFile): ExecutionResult[] { return [new PCWriter(rf.readRegister(LR_INDEX))]; }
};
