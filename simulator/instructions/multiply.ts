import { ArithmeticInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { RegisterWriter } from "../components/execution-result";
import { HasRegisters, lookupRegisters } from "../components/register-file";
import { ArithmeticInteractions } from "../components/instruction-interactions";

export class Multiply extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r12: Register[];
  readonly i3: Literal;

  static readonly pneumonic: string = "mult";

  get duration(): number { return 2; }

  get requirements(): ArithmeticInteractions { return new ArithmeticInteractions(this.r12); }

  get effects(): ArithmeticInteractions { return new ArithmeticInteractions([this.r0]); }

  execute(rf: HasRegisters): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        lookupRegisters(rf, this.r12).reduce((acc, item) => acc * item, this.i3))
    ];
  }
};
