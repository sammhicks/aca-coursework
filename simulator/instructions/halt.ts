import { MiscInstruction } from "./instruction";
import { ExecutionResult, Halter } from "../components/execution-result";
import { LikeRegisterFile } from "../components/register-file";
import { InstructionInteractions, NoInteractions } from "../components/instruction-interactions";

export class Halt extends MiscInstruction {
  static readonly pneumonic: string = "halt";

  get duration(): number { return 1; }

  get halts(): boolean { return true; }

  get requirements(): InstructionInteractions { return new NoInteractions(); }

  get effects(): InstructionInteractions { return new NoInteractions(); }

  execute(rf: LikeRegisterFile): ExecutionResult[] { return [new Halter()]; }
};
