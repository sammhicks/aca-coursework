import { Register } from "./register"

export class InstructionInteractions {
  constructor(public registers: Register[] = [], public pc: boolean = false, public lr: boolean = false, public memory: boolean = false) { }
}
