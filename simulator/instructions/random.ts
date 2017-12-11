import { IOInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { RegisterWriter, ExternalAction } from "../components/execution-result";
import { HasRegisters } from "../components/register-file";
import { IOInteractions } from "../components/instruction-interactions";

export class Random extends IOInstruction {
  private r0: Register;
  private a1: Literal;
  private b2: Literal;

  static readonly pneumonic: string = "rand";

  get duration() { return 4; }

  get requirements() { return new IOInteractions([]); }

  get effects() { return new IOInteractions([this.r0]); }

  execute(rf: ReadableRegisterFile) {
    const generatedValue = this.a1 + Math.floor(Math.random() * (this.b2 - this.a1));

    return [
      new RegisterWriter(this.r0, generatedValue),
      new ExternalAction(() => console.log("Random: ", generatedValue))];
  }
};
