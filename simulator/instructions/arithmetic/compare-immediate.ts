import { ArithmeticInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { RegisterWriter } from "../../components/execution-result";
import { ReadsRegister, SetsRegister } from "../../components/instruction-requirements"
import { HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";
import { compare } from "../../util/compare";

export class CompareImmediate extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r1: Register;
  readonly i2: Literal;

  static readonly pneumonic: string = "cmpi";

  get duration() { return 1; }

  getReadRequirements(sync: RegisterSync) { return [new ReadsRegister(sync, this.r1)]; }

  getWriteRequirements(sync: RegisterSync) { return [new SetsRegister(sync, this.r0)]; }

  execute(rf: HasRegisters) {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.getRegister(this.r1), this.i2))
    ];
  }
};
