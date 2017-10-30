import { Countdown } from "./countdown";
import { Instruction } from "../instructions/instruction";
import { RegisterFile } from "./register-file";

export class ExecutionUnit extends Countdown {
  private _rf: RegisterFile;
  private _instruction: Instruction | null;

  constructor(rf: RegisterFile) {
    super();

    this._rf = rf;
    this._instruction = null;
  }

  executeInstruction(instruction: Instruction) {
    this._instruction = instruction;
    this.reset(instruction.duration());
  }

  onCompletion(): void {
    if (this._instruction != null) {
      const writes = this._instruction.execute(this._rf);

      for (var index = 0; index < writes.length; index++) {
        writes[index].write(this._rf);
      }
    }
  }
}
