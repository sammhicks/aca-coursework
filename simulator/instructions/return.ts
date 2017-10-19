import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, PCWriter, LR_INDEX } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions, PCInteractions } from "../components/instruction-interactions";

export class Return extends Instruction {
  static pneumonic: string = "ret";

  duration(): number { return 1; }

  requirements(): InstructionInteractions { return new RegisterInteractions([LR_INDEX]); }

  effects(): InstructionInteractions { return new PCInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] { return [new PCWriter(rf.registers[LR_INDEX])]; }
};
