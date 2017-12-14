import { ArithmeticInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { RegisterWriter } from "../../components/execution-result";
import { ReadsRegister, SetsRegister } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";
import { compare } from "../../util/compare";

export class Compare extends ArithmeticInstruction {
  readonly r0: Register;
  readonly r1: Register;
  readonly r2: Register;

  static readonly pneumonic: string = "cmp";

  get duration() { return 1; }

  getReadRequirements(sync: RegisterSync) { return [this.r1, this.r2].map(r => new ReadsRegister(sync, r)); }

  getWriteRequirements(sync: RegisterSync) { return [new SetsRegister(sync, this.r0)]; }

  execute(rf: HasRegisters): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.getRegister(this.r1), rf.getRegister(this.r2)))
    ];
  }
};
