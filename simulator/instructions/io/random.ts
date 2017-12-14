import { IOInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { ExternalAction, RegisterWriter } from "../../components/execution-result";
import { ReadsRegister, SetsRegister } from "../../components/instruction-requirements"
import { getRegisters, HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Random extends IOInstruction {
  private r0: Register;
  private a1: Literal;
  private b2: Literal;

  static readonly pneumonic: string = "rand";

  get duration() { return 4; }

  getReadRequirements() { return [] as never[]; }

  getWriteRequirements(sync: RegisterSync) { return [new SetsRegister(sync, this.r0)]; }

  execute(rf: HasRegisters) {
    const generatedValue = this.a1 + Math.floor(Math.random() * (this.b2 - this.a1));

    return [
      new RegisterWriter(this.r0, generatedValue),
      new ExternalAction(() => console.log("Random: ", generatedValue))];
  }
};
