import { ArithmeticInstruction } from "../instruction";
import { Register } from "../../components/basic-types";
import { RegisterWriter, RegisterReleaser, registerReleasers } from "../../components/execution-result";
import { registerInteractions } from "../../components/instruction-requirements"
import { HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";
import { compare } from "../../util/compare";

export class Compare extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r1: Register;
  readonly r2: Register;

  static readonly pneumonic: string = "cmp";

  get duration() { return 1; }

  getRequirements(sync: RegisterSync) { return registerInteractions(sync, this.r0, [this.r1, this.r2]); }

  execute(rf: HasRegisters) {
    return ([] as (RegisterWriter | RegisterReleaser)[])
      .concat([new RegisterWriter(this.r0, compare(rf.getRegister(this.r1), rf.getRegister(this.r2)))])
      .concat(registerReleasers(this.r0, [this.r1, this.r2]));
  }
};
