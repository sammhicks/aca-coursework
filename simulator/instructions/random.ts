import { MiscInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { ExecutionResult, RegisterWriter, ExternalAction } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, NoInteractions, RegisterInteractions } from "../components/instruction-interactions";

export class Random extends MiscInstruction {
  private r0: Register;
  private a1: Literal;
  private b2: Literal;

  static readonly pneumonic: string = "rand";

  get duration(): number { return 4; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    const generatedValue = this.a1 + Math.floor(Math.random() * (this.b2 - this.a1));

    return [
      new RegisterWriter(this.r0, generatedValue),
      new ExternalAction(() => console.log("Random: ", generatedValue))];
  }
};
