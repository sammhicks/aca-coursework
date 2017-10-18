import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, MemoryWriter } from "../components/register-file";
import { InstructionRequirements } from "../components/instruction-requirements";

export class Store extends Instruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  duration: number = 4;

  requirements(): InstructionRequirements { return new InstructionRequirements(this.r12, false, false, true); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new MemoryWriter(
        rf.lookupRegisters(this.r12).reduce((acc, item) => acc + item, this.i3),
        rf.registers[this.r0])
    ];
  }
};
