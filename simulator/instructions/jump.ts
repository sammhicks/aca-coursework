import { Instruction } from "./instruction";
import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, PCWriter } from "../components/register-file";
import { InstructionRequirements } from "../components/instruction-requirements";

export class Jump extends Instruction {
  private i0: Literal;

  duration: number = 1;

  requirements(): InstructionRequirements { return new InstructionRequirements([], true); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new PCWriter(rf.pc + this.i0)]; }
};
