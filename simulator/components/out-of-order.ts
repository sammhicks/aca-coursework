import { PC, Register, Literal, Address, Registers, Memory } from "./basic-types";
import { ExecutionUnits } from "./execution-unit";
import { InstructionFetcher } from "./instruction-fetcher";
import { InstructionRequirement } from "./instruction-requirements";
import { RegisterFile } from "./register-file";
import { RegisterFileSync, MemorySlot } from "./register-file-sync";
import { ReorderBufferSlot, ReorderBuffer } from "./reorder-buffer";
import { Instruction } from "../instructions/instruction";
import { Prediction } from "./prediction";


class ReservationStationSlot {
  readonly instructionRequirements: InstructionRequirement[];

  constructor(readonly rf: RegisterFile, readonly sync: RegisterFileSync, readonly pc: PC, readonly instruction: Instruction, readonly reorderBufferSlot: ReorderBufferSlot) {
    this.instructionRequirements = instruction.getRequirements(sync, rf);
    this.instructionRequirements.forEach(req => req.updateSync());
  }
}

export class ReservationStation extends RegisterFile {
  private _instructionFetcher: InstructionFetcher
  private _prediction: Prediction;
  private _slots: ReservationStationSlot[];
  private _sync: RegisterFileSync;
  private _executionUnits: ExecutionUnits;
  private _reorderBuffer: ReorderBuffer;
  private _running: boolean;

  constructor(readonly slotCount: number, memorySlotCount: MemorySlot, instructionFetcher: InstructionFetcher, prediction: Prediction, executionUnits: ExecutionUnits, reorderBuffer: ReorderBuffer) {
    super();
    this._instructionFetcher = instructionFetcher;
    this._prediction = prediction;
    this._slots = [];
    this._sync = new RegisterFileSync(memorySlotCount);
    this._executionUnits = executionUnits;
    this._reorderBuffer = reorderBuffer;
    this._running = true;
  }

  loadInstructions() {
    while (this._running && this._slots.length < this.slotCount) {
      const pcAndInstruction = this._instructionFetcher.load();
      const newReorderBufferSlot = this._reorderBuffer.newSlot();
      this._slots.push(new ReservationStationSlot(this, this._sync, pcAndInstruction[0], pcAndInstruction[1], newReorderBufferSlot));
      if (pcAndInstruction[1].halts) {
        this._running = false;
      }
    }
  }

  executeInstructions() {
    const executionUnits = this._executionUnits;
    this._slots = this._slots.filter(function (slot: ReservationStationSlot) {
      slot.instructionRequirements.forEach(req => req.updateSync());

      if (slot.instructionRequirements.every(req => req.isMet())) {
        if (executionUnits.executeInstruction(slot.rf, slot.pc, slot.instruction, [slot.reorderBufferSlot, slot.rf])) {
          return false;
        }
      }

      return true;
    });
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

  halt() { this._running = false; }

  notifyBranchTaken(pc: PC, branchTaken: boolean) {
    if (this._running) {
      this._prediction.branchPrediction.updateValue(pc, branchTaken);
    }
  }

  handleBranchPredictionSuccess() { }

  handleBranchPredictionError() { this._running = false; }

  notifyReturn(pc: PC, ret: PC) {
    if (this._running) {
      this._prediction.returnPrediction.updateValue(pc, ret);
    }
  }

  handleReturnPredictionSuccess() { }

  handleReturnPredictionError() { this._running = false; }

  reset(regs: Registers, memory: Memory) {
    this._sync.reset();
    this._slots = [];
    this._registers = regs.slice();
    this._memory = memory.slice();
    this._running = true;
  }
}
