import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionRequirements } from "../components/instruction-requirements";

export class Load extends Instruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  duration: number = 4;

  requirements(): InstructionRequirements { return new InstructionRequirements(this.r12, false, false, true); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.memory[rf.lookupRegisters(this.r12).reduce((acc, item) => acc + item, this.i3)])
    ];
  }
};
