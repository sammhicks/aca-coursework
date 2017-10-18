import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Rand extends Instruction {
  private r0: Register;
  private a: Literal;
  private b: Literal;

  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions(); }

  effects(): InstructionInteractions { return new InstructionInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        this.a + Math.floor(Math.random() * (this.b - this.a)))
    ];
  }
};
