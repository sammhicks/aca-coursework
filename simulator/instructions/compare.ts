import { Instruction } from "./instruction";
import { Register } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions } from "../components/instruction-interactions";
import { compare } from "../util";

export class Compare extends Instruction {
  private r0: Register;
  private r1: Register;
  private r2: Register;

  static pneumonic: string = "cmp";

  duration(): number { return 1; }

  requirements(): InstructionInteractions { return new RegisterInteractions([this.r1, this.r2]); }

  effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.registers[this.r1], rf.registers[this.r2]))
    ];
  }
};
