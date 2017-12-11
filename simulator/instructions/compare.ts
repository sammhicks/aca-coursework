import { ArithmeticInstruction } from "./instruction";
import { Register } from "../components/basic-types";
import { RegisterWriter } from "../components/execution-result";
import { HasRegisters } from "../components/register-file";
import { ArithmeticInteractions } from "../components/instruction-interactions";
import { compare } from "../util/compare";

export class Compare extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r1: Register;
  readonly r2: Register;

  static readonly pneumonic: string = "cmp";

  get duration() { return 1; }

  get requirements() { return new ArithmeticInteractions([this.r1, this.r2]); }

  get effects() { return new ArithmeticInteractions([this.r0]); }

  execute(rf: ReadableRegisterFile): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.readRegister(this.r1), rf.readRegister(this.r2)))
    ];
  }
};
