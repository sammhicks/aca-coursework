import { ArithmeticInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { RegisterWriter } from "../components/execution-result";
import { ReadableRegisterFile, getRegisters } from "../components/register-file";
import { ArithmeticInteractions } from "../components/instruction-interactions";

export class Add extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r12: Register[];
  readonly i3: Literal;

  static readonly pneumonic: string = "add";

  get duration() { return 1; }

  get requirements() { return new ArithmeticInteractions(this.r12); }

  get effects() { return new ArithmeticInteractions([this.r0]); }

  execute(rf: ReadableRegisterFile): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        getRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3))
    ];
  }
};
