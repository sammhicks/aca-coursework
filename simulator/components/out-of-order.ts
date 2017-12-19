import { PC, Register, Literal, Address } from "./basic-types";
import { InstructionFetcher } from "./instruction-fetcher";
import { HandlesBranchPredictionError, ReadableRegisterFile, WritableRegisterFile, RegisterFile } from "./register-file";
import { ReorderBufferSlot, ReorderBuffer } from "./reorder-buffer";
import { Instruction } from "../instructions/instruction";
import { InstructionRequirement } from "./instruction-requirements";
import { RegisterFileSync, MemorySlot } from "./register-file-sync";


class ReservationStationSlot {
  readonly instructionRequirements: InstructionRequirement[];

  constructor(readonly instruction: Instruction, readonly sync: RegisterFileSync, readonly rf: ReadableRegisterFile, readonly reorderBufferSlot: ReorderBufferSlot) {
    this.instructionRequirements = ([] as InstructionRequirement[]).concat(...instruction.getReadRequirements(sync, rf), ...instruction.getWriteRequirements(sync, rf));
  }
}

export class ReservationStation extends RegisterFile {
  private _instructionFetcher: InstructionFetcher
  private _slots: ReservationStationSlot[];
  private _sync: RegisterFileSync;
  private _reorderBuffer: ReorderBuffer;

  constructor(readonly slotCount: number, memorySlotCount: MemorySlot, instructionFetcher: InstructionFetcher, reorderBuffer: ReorderBuffer) {
    super();
    this._instructionFetcher = instructionFetcher;
    this._slots = [];
    this._sync = new RegisterFileSync(memorySlotCount);
    this._reorderBuffer = reorderBuffer;
  }

  loadInstructions() {
    while (this._slots.length < this.slotCount) {
      const newInstruction = this._instructionFetcher.load();
      const newReorderBufferSlot = this._reorderBuffer.newSlot();
      this._slots.push(new ReservationStationSlot(newInstruction, this._sync, this, newReorderBufferSlot));
    }
  }


  executeInstructions() {
    var remainingInstructions: Instruction[] = [];
  }


  updateRegister(reg: Register, val: Literal) {
    super.updateRegister(reg, val);

    this._sync.getRegisterSync(reg).currentState.increment();
  }

  releaseRegister(reg: Register) {
    this._sync.getRegisterSync(reg).readersState.decrement();
  }

  writeMemory(addr: Address, val: Literal) {
    super.writeMemory(addr, val);

    this._sync.getMemorySync(addr).currentState.increment();
  }

  releaseMemory(addr: Address) {
    this._sync.getMemorySync(addr).readersState.decrement();
  }

  performExternalAction() { }

  halt() { }

  handleBranchPredictionError(pc: PC) {
    this._slots = [];
  }
}
