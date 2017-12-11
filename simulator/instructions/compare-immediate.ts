import { ArithmeticInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { RegisterWriter } from "../components/execution-result";
import { HasRegisters } from "../components/register-file";
import { ArithmeticInteractions } from "../components/instruction-interactions";
import { compare } from "../util/compare";

export class CompareImmediate extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r1: Register;
  readonly i2: Literal;

  static readonly pneumonic: string = "cmpi";

  get duration() { return 1; }

  get requirements() { return new ArithmeticInteractions([this.r1]); }

  get effects() { return new ArithmeticInteractions([this.r0]); }

  execute(rf: HasRegisters): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.getRegister(this.r1), this.i2))
    ];
  }
};
