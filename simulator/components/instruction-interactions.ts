import { Register } from "./basic-types"

export interface InstructionInteractions { }

export interface HasPCInteractions extends InstructionInteractions {
  pcFlag: boolean;
}

export interface HasRegisterInteractions extends InstructionInteractions {
  rs: Register[];
}

export interface HasMemoryInteractions extends InstructionInteractions {
  addrFlag: boolean;
}

export class ArithmeticInteractions implements HasRegisterInteractions {
  constructor(public rs: Register[]) { }
}

export class MemoryInteractions implements HasRegisterInteractions, HasMemoryInteractions {
  constructor(public rs: Register[], public addrFlag: boolean) { }
}

export class BranchInteractions implements HasPCInteractions, HasRegisterInteractions {
  constructor(public pcFlag: boolean, public rs: Register[]) { }
}

export class IOInteractions implements HasRegisterInteractions {
  constructor(public rs: Register[]) { }
}

export class MiscInteractions implements InstructionInteractions { }
