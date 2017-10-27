import { Countdown } from "./countdown";
import { Instruction } from "../instructions/instruction";
import { RegisterFile } from "./register-file";

export class ExecutionUnit extends Countdown {
  private instruction: Instruction | null;

  constructor(private rf: RegisterFile) {
    super();

    this.instruction = null;
  }

  executeInstruction(instruction: Instruction) {
    this.instruction = instruction;
    this.reset(instruction.duration());
  }

  onCompletion(): void {
    if (this.instruction != null) {
      const writes = this.instruction.execute(this.rf);

      for (var index = 0; index < writes.length; index++) {
        writes[index].write(this.rf);
      }
    }
  }
}
