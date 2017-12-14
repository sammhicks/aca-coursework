import { IOInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { ExternalAction } from "../../components/execution-result";
import { ReadsRegister, SetsRegister } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Log extends IOInstruction {
  readonly message: string;

  static readonly pneumonic: string = "log";

  get duration() { return 1; }

  getReadRequirements() { return [] as never[]; }

  getWriteRequirements() { return [] as never[]; }

  execute() {
    const self = this;
    return [new ExternalAction(() => console.log("Log: \"%s\"", self.message))];
  }
};
