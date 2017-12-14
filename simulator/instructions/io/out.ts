import { IOInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { ExternalAction, RegisterWriter } from "../../components/execution-result";
import { ReadsRegister, SetsRegister } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Out extends IOInstruction {
  readonly label: string;
  readonly r0: Register;

  static readonly pneumonic: string = "out";

  get duration() { return 1; }

  getReadRequirements(sync: RegisterSync) { return [new ReadsRegister(sync, this.r0)]; }

  getWriteRequirements() { return [] as never[]; }

  execute(rf: HasRegisters) {
    const self = this;
    return [new ExternalAction(() => console.log("Out: %s (%d) = %d", self.label, self.r0, rf.getRegister(self.r0)))];
  }
};
