import { Instruction } from "./instruction";
import { Literal } from "../components/register";
import { RegisterFile, RegisterFileWriter, RegisterWriter, PCWriter, LR_INDEX } from "../components/register-file";
import { InstructionInteractions, NoInteractions, PCInteractions } from "../components/instruction-interactions";

export class Branch extends Instruction {
  private i0: Literal;

  static pneumonic: string = "b";

  get duration(): number { return 2; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new PCInteractions([LR_INDEX]); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    return [
      new RegisterWriter(LR_INDEX, rf.pc),
      new PCWriter(this.i0)
    ];
  }
};
