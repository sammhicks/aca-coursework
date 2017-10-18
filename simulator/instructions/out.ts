import { Instruction } from "./instruction";
import { Register } from "../components/register";
import { RegisterFile, RegisterFileWriter, ExternalAction } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Out extends Instruction {
  private label: string;
  private r0: Register;

  static pneumonic: string = "out";

  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions([this.r0]); }

  effects(): InstructionInteractions { return new InstructionInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    const self = this;
    return [new ExternalAction(function action(rf: RegisterFile) {
      console.log("Out: %s (%d) = %d", self.label, self.r0, rf.registers[self.r0]);
    })];
  }
};
