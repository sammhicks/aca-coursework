import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, ExternalAction } from "../components/register-file";
import { InstructionInteractions } from "../components/instruction-interactions";

export class Log extends Instruction {
  private message: string;

  duration: number = 1;

  requirements(): InstructionInteractions { return new InstructionInteractions(); }

  effects(): InstructionInteractions { return new InstructionInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    const self = this;
    return [new ExternalAction(function action(rf: RegisterFile) {
      console.log("Log: \"%s\"", self.message);
    })];
  }
};
