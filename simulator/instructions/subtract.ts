import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions } from "../components/instruction-interactions";

export class Subtract extends Instruction {
  private r0: Register;
  private r1: Register;
  private r2: Register | null;
  private i3: Literal;

  static pneumonic: string = "sub";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new RegisterInteractions(this.r2 == null ? [this.r1] : [this.r1, this.r2]); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.registers[this.r1] - (this.r2 == null ? 0 : rf.registers[this.r2]) - this.i3)
    ];
  }
};
