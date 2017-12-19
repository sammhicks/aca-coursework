import { ArithmeticInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { RegisterWriter, RegisterReleaser, registerReleasers } from "../../components/execution-result";
import { registerInteractions } from "../../components/instruction-requirements"
import { HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Subtract extends ArithmeticInstruction {
  private r0: Register;
  private r1: Register;
  private r2: Register | null;
  private i3: Literal;

  static readonly pneumonic: string = "sub";

  get duration(): number { return 1; }

  getRequirements(sync: RegisterSync) { return registerInteractions(sync, this.r0, [this.r1, this.r2]); }

  execute(rf: HasRegisters) {
    return ([] as (RegisterWriter | RegisterReleaser)[])
      .concat([new RegisterWriter(this.r0, rf.getRegister(this.r1) - (this.r2 == null ? 0 : rf.getRegister(this.r2)) - this.i3)])
      .concat(registerReleasers(this.r0, [this.r1, this.r2]));
  }
};
