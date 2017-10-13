import { Instruction } from "./instruction";
import { Literal, RegisterFile, RegisterFileWriter, LRWriter, PCWriter, RegisterRequirements } from "../components";

export class Branch extends Instruction {
  private i0: Literal;

  requirements(): RegisterRequirements { return new RegisterRequirements([], true, true); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new LRWriter(rf.pc + 1),
      new PCWriter(this.i0)
    ];
  }
};
