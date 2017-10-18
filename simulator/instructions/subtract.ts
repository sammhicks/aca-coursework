import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionRequirements } from "../components/instruction-requirements";

export class Subtract extends Instruction {
  private r0: Register;
  private r1: Register;
  private r2: Register | null;
  private i3: Literal

  duration: number = 1;

  requirements(): InstructionRequirements { return new InstructionRequirements(this.r2 == null ? [this.r1] : [this.r1, this.r2]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.registers[this.r1] - (this.r2 == null ? 0 : rf.registers[this.r2]) - this.i3)
    ];
  }
};