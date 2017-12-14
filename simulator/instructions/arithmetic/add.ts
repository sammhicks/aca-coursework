import { ArithmeticInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { RegisterWriter } from "../../components/execution-result";
import { ReadsRegister, SetsRegister } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Add extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r12: Register[];
  readonly i3: Literal;

  static readonly pneumonic: string = "add";

  get duration() { return 1; }

  getReadRequirements(sync: RegisterSync) { return this.r12.map(r => new ReadsRegister(sync, r)); }

  getWriteRequirements(sync: RegisterSync) { return [new SetsRegister(sync, this.r0)]; }

  execute(rf: HasRegisters) {
    return [
      new RegisterWriter(
        this.r0,
        getRegisters(rf, this.r12).reduce((acc, item) => acc + item, this.i3))
    ];
  }
};
