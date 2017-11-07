import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, PCWriter, LR_INDEX } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions, PCInteractions } from "../components/instruction-interactions";

export class Return extends Instruction {
  static pneumonic: string = "ret";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new RegisterInteractions([LR_INDEX]); }

  get effects(): InstructionInteractions { return new PCInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new PCWriter(rf.registers[LR_INDEX])]; }
};
