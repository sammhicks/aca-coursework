import { ArithmeticInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { ExecutionResult, RegisterWriter } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions } from "../components/instruction-interactions";
import { compare } from "../util/compare";

export class CompareImmediate extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r1: Register;
  readonly i2: Literal;

  static readonly pneumonic: string = "cmpi";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new RegisterInteractions([this.r1]); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.readRegister(this.r1), this.i2))
    ];
  }
};
