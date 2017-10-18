import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, PCWriter } from "../components/register-file";
import { InstructionRequirements } from "../components/instruction-requirements";

export class Add extends Instruction {
  duration: number = 1;

  requirements(): InstructionRequirements { return new InstructionRequirements(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new PCWriter(rf.lr)]; }
};
