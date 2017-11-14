import { Register } from "./basic-types"

export class InstructionInteractions {
  constructor(readonly registers: Register[] = [], readonly pc: boolean, readonly memory: boolean) { }
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
