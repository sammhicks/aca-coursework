import { Instruction } from "./instruction";
import { RegisterFile, RegisterFileWriter, ExternalAction } from "../components/register-file";
import { InstructionInteractions, NoInteractions } from "../components/instruction-interactions";

export class Log extends Instruction {
  private message: string;

  static pneumonic: string = "log";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new NoInteractions(); }

  execute(rf: RegisterFile): RegisterFileWriter[] {
    const self = this;
    return [new ExternalAction(function action(rf: RegisterFile) {
      console.log("Log: \"%s\"", self.message);
    })];
  }
};
