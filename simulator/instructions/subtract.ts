import { ArithmeticInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { ExecutionResult, RegisterWriter } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions } from "../components/instruction-interactions";

export class Subtract extends ArithmeticInstruction {
  private r0: Register;
  private r1: Register;
  private r2: Register | null;
  private i3: Literal;

  static readonly pneumonic: string = "sub";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new RegisterInteractions(this.r2 == null ? [this.r1] : [this.r1, this.r2]); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.readRegister(this.r1) - (this.r2 == null ? 0 : rf.readRegister(this.r2)) - this.i3)
    ];
  }
};
