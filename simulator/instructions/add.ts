import { Instruction } from "./instruction";
import { Register, Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions } from "../components/instruction-interactions";

export class Add extends Instruction {
  private r0: Register;
  private r12: Register[];
  private i3: Literal;

  static pneumonic: string = "add";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new RegisterInteractions(this.r12); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(
        this.r0,
        rf.lookupRegisters(this.r12).reduce((acc, item) => acc + item, this.i3))
    ];
  }
};
