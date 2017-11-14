import { ArithmeticInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { RegisterWriter } from "../components/execution-result";
import { HasRegisters } from "../components/register-file";
import { ArithmeticInteractions } from "../components/instruction-interactions";

export class Subtract extends ArithmeticInstruction {
  private r0: Register;
  private r1: Register;
  private r2: Register | null;
  private i3: Literal;

  static readonly pneumonic: string = "sub";

  get duration(): number { return 1; }

  get requirements(): ArithmeticInteractions { return new ArithmeticInteractions(this.r2 == null ? [this.r1] : [this.r1, this.r2]); }

  get effects(): ArithmeticInteractions { return new ArithmeticInteractions([this.r0]); }

  execute(rf: HasRegisters): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        rf.readRegister(this.r1) - (this.r2 == null ? 0 : rf.readRegister(this.r2)) - this.i3)
    ];
  }
};
