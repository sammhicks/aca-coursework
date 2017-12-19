import { ArithmeticInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { RegisterWriter, RegisterReleaser, registerReleasers } from "../../components/execution-result";
import { registerInteractions } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Add extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r12: Register[];
  readonly i3: Literal;

  static readonly pneumonic: string = "add";

  get duration() { return 1; }

  getRequirements(sync: RegisterSync) { return registerInteractions(sync, this.r0, this.r12); }

  execute(rf: HasRegisters) {
    return ([] as (RegisterWriter | RegisterReleaser)[])
      .concat([new RegisterWriter(this.r0, getRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3))])
      .concat(registerReleasers(this.r0, this.r12));
  }
};
