import { Instruction } from "./instruction";
import { Register, Literal, RegisterWriter, RegisterFile, RegisterFileWriter, RegisterRequirements } from "../components";

export class Add extends Instruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal

  requirements(): RegisterRequirements { return new RegisterRequirements(this.r12); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.lookupRegisters(this.r12).reduce((acc, item) => acc + item, this.i3))
    ];
  }
};
