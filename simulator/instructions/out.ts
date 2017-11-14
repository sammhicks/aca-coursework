import { IOInstruction } from "./instruction";
import { Register } from "../components/basic-types";
import { ExecutionResult, ExternalAction } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, NoInteractions, RegisterInteractions } from "../components/instruction-interactions";

export class Out extends IOInstruction {
  readonly label: string;
  readonly r0: Register;

  static readonly pneumonic: string = "out";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  get effects(): InstructionInteractions { return new NoInteractions(); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    const self = this;
    return [new ExternalAction(() => console.log("Out: %s (%d) = %d", self.label, self.r0, rf.readRegister(self.r0)))];
  }
};
