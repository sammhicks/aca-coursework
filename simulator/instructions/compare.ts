import { ArithmeticInstruction } from "./instruction";
import { Register, Literal } from "../components/basic-types";
import { ExecutionResult, RegisterWriter } from "../components/execution-result";
import { LikeRegisterFile, lookupRegisters } from "../components/register-file";
import { InstructionInteractions, RegisterInteractions } from "../components/instruction-interactions";
import { compare } from "../util/compare";

export class Compare extends ArithmeticInstruction {
  private r0: Register;
  private r1: Register;
  private r2: Register;

  static readonly pneumonic: string = "cmp";

  get duration(): number { return 1; }

  get requirements(): InstructionInteractions { return new RegisterInteractions([this.r1, this.r2]); }

  get effects(): InstructionInteractions { return new RegisterInteractions([this.r0]); }

  execute(rf: LikeRegisterFile): ExecutionResult[] {
    return [
      new RegisterWriter(
        this.r0,
        compare(rf.readRegister(this.r1), rf.readRegister(this.r2)))
    ];
  }
};
