import { ArithmeticInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { RegisterWriter } from "../../components/execution-result";
import { ReadsRegister, SetsRegister } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Subtract extends ArithmeticInstruction {
  private r0: Register;
  private r1: Register;
  private r2: Register | null;
  private i3: Literal;

  static readonly pneumonic: string = "sub";

  get duration(): number { return 1; }

  getReadRequirements(sync: RegisterSync) { return (this.r2 == null ? [this.r1] : [this.r1, this.r2]).map(r => new ReadsRegister(sync, r)); }

  getWriteRequirements(sync: RegisterSync) { return [new SetsRegister(sync, this.r0)]; }

  execute(rf: HasRegisters): [RegisterWriter] {
    return [
      new RegisterWriter(
        this.r0,
        rf.getRegister(this.r1) - (this.r2 == null ? 0 : rf.getRegister(this.r2)) - this.i3)
    ];
  }
};
