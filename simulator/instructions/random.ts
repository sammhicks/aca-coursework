import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter, ExternalAction } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Random extends Instruction {
  private r0: Register;
  private a1: Literal;
  private b2: Literal;

  static pneumonic: string = "rand";

  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions(); }

  effects(): InstructionInteractions { return new InstructionInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    const generatedValue = this.a1 + Math.floor(Math.random() * (this.b2 - this.a1));

    return [
      new RegisterWriter(this.r0, generatedValue),
      new ExternalAction(function action(rf: RegisterFile) {
        console.log("Random: ", generatedValue);
      })];
  }
};
