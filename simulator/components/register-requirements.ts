import { Register } from "./register"

export class RegisterRequirements {
  constructor(public registers: Register[] = [], public pc: boolean = false, public lr: boolean = false) { }
}
