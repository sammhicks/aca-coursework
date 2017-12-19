import { IOInstruction } from "../instruction";
import { Register } from "../../components/basic-types";
import { ExternalAction, RegisterReleaser } from "../../components/execution-result";
import { ReadsRegister } from "../../components/instruction-requirements"
import { HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Out extends IOInstruction {
  readonly label: string;
  readonly r0: Register;

  static readonly pneumonic: string = "out";

  get duration() { return 1; }

  getRequirements(sync: RegisterSync) { return [new ReadsRegister(sync, this.r0)]; }

  execute(rf: HasRegisters) {
    const self = this;
    return [
      new RegisterReleaser(this.r0),
      new ExternalAction(() => console.log("Out: %s (%d) = %d", self.label, self.r0, rf.getRegister(self.r0)))
    ];
  }
};
