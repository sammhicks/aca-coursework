import { Instruction } from "./instruction";
import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, LRWriter, PCWriter } from "../components/register-file";
import { InstructionRequirements } from "../components/instruction-requirements";

export class Branch extends Instruction {
  private i0: Literal;

  duration: number = 1;

  requirements(): InstructionRequirements { return new InstructionRequirements([], true, true); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new LRWriter(rf.pc + 1),
      new PCWriter(this.i0)
    ];
  }
};
