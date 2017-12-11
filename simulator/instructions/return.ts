import { BranchInstruction } from "./instruction";
import { PCWriter } from "../components/execution-result";
import { HasRegisters, LR_INDEX } from "../components/register-file";
import { BranchInteractions } from "../components/instruction-interactions";

export class Return extends BranchInstruction {
  static readonly pneumonic: string = "ret";

  get duration() { return 1; }

  get requirements() { return new BranchInteractions(true, [LR_INDEX]); }

  get effects() { return new BranchInteractions(true, []); }

  execute(rf: ReadableRegisterFile) { return [new PCWriter(rf.readRegister(LR_INDEX))]; }
};
