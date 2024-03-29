import { IOInstruction } from "../instruction";
import { Register, Literal } from "../../components/basic-types";
import { ExternalAction, RegisterWriter } from "../../components/execution-result";
import { SetsRegister } from "../../components/instruction-requirements"
import { HasRegisters } from "../../components/register-file";
import { RegisterSync } from "../../components/register-file-sync";

export class Random extends IOInstruction {
  private r0: Register;
  private a1: Literal;
  private b2: Literal;

  static readonly pneumonic: string = "rand";

  get duration() { return 4; }

  getRequirements(sync: RegisterSync) { return [new SetsRegister(sync, this.r0)]; }

  execute(rf: HasRegisters) {
    const generatedValue = this.a1 + Math.floor(Math.random() * (this.b2 - this.a1));

    return [
      new ExternalAction(() => console.log("Random: ", generatedValue)),
      new RegisterWriter(this.r0, generatedValue)
    ];
  }
};
