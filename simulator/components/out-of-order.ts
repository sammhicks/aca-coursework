import { PC, Register, Literal, Address } from "./basic-types";
import { InstructionFetcher } from "./instruction-fetcher";
import { RegisterFile } from "./register-file";
import { ReorderBufferSlot, ReorderBuffer } from "./reorder-buffer";
import { Instruction } from "../instructions/instruction";
import { InstructionRequirement } from "./instruction-requirements";
import { RegisterFileSync, MemorySlot } from "./register-file-sync";
import { ExecutionUnits } from "./execution-unit";


class ReservationStationSlot {
  readonly instructionRequirements: InstructionRequirement[];

  constructor(readonly rf: RegisterFile, readonly sync: RegisterFileSync, readonly pc: PC, readonly instruction: Instruction, readonly reorderBufferSlot: ReorderBufferSlot) {
    this.instructionRequirements = instruction.getRequirements(sync, rf);
  }
}

export class ReservationStation extends RegisterFile {
  private _instructionFetcher: InstructionFetcher
  private _slots: ReservationStationSlot[];
  private _sync: RegisterFileSync;
  private _executionUnits: ExecutionUnits;
  private _reorderBuffer: ReorderBuffer;

  constructor(readonly slotCount: number, memorySlotCount: MemorySlot, instructionFetcher: InstructionFetcher, executionUnits: ExecutionUnits, reorderBuffer: ReorderBuffer) {
    super();
    this._instructionFetcher = instructionFetcher;
    this._slots = [];
    this._sync = new RegisterFileSync(memorySlotCount);
    this._executionUnits = executionUnits;
    this._reorderBuffer = reorderBuffer;
  }

  loadInstructions() {
    while (this._slots.length < this.slotCount) {
      const pcAndInstruction = this._instructionFetcher.load();
      const newReorderBufferSlot = this._reorderBuffer.newSlot();
      this._slots.push(new ReservationStationSlot(this, this._sync, pcAndInstruction[0], pcAndInstruction[1], newReorderBufferSlot));
    }
  }

  executeInstructions() {
    const executionUnits = this._executionUnits;
    this._slots = this._slots.filter(slot => !executionUnits.executeInstruction(slot.rf, slot.pc, slot.instruction, [slot.reorderBufferSlot, slot.rf]));
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

  handleBranchPredictionError(pc: PC) { }

  reset() {
    this._sync.reset();
    this._slots = [];
  }
}
