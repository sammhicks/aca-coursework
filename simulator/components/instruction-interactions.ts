import { Register } from "./register"

export class InstructionInteractions {
  constructor(public registers: Register[] = [], public pc: boolean, public memory: boolean) { }
}

export class RegisterInteractions extends InstructionInteractions {
  constructor(registers: Register[]) { super(registers, false, false); }
}

export class NoInteractions extends InstructionInteractions {
  constructor() { super([], false, false); }
}

export class PCInteractions extends InstructionInteractions {
  constructor(registers: Register[] = []) { super(registers, true, false); }
}

export class MemoryInteractions extends InstructionInteractions {
  constructor(registers: Register[]) { super(registers, false, true); }
}
