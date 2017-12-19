import { PC } from "./basic-types";
import { ReservationStation } from "./out-of-order";
import { RegisterFile } from "./register-file";
import { InstructionFetcher } from "./instruction-fetcher";
import { Instruction } from "../instructions/instruction";
import { ExecutionUnits } from "./execution-unit";
import { ReorderBuffer } from "./reorder-buffer";


export class Processor extends RegisterFile {
  private _fetcher: InstructionFetcher;
  private _reservationStation: ReservationStation;
  private _executionUnits: ExecutionUnits;
  private _reorderBuffer: ReorderBuffer;

  private _running: boolean
  private _clockCycles: number;
  private _instructionsExecuted: number;

  constructor(reservationStationCount: number, memorySlotCount: number, arithmeticExecutionUnitCount: number, memoryExecutionUnitCount: number, instructions: Instruction[]) {
    super();

    this._fetcher = new InstructionFetcher(instructions)

    this._executionUnits = new ExecutionUnits(arithmeticExecutionUnitCount, memoryExecutionUnitCount);

    this._reorderBuffer = new ReorderBuffer(this);

    this._reservationStation = new ReservationStation(reservationStationCount, memorySlotCount, this._fetcher, this._executionUnits, this._reorderBuffer);

    this._running = true;
    this._clockCycles = 0;
    this._instructionsExecuted = 0;
  }

  releaseRegister() { }

  releaseMemory() { }

  performExternalAction(action: () => void) { action(); }

  get isRunning() { return this._running; }

  halt() {
    this._running = false;
  }

  handleBranchPredictionError(pc: PC) {
    this._fetcher.reset(pc);
    this._reservationStation.reset();
    this._executionUnits.resetUnits();
    this._reorderBuffer.reset();
  }

  run() {
    while (this.isRunning) {
      this._clockCycles += 1;

      this._reservationStation.loadInstructions();
      this._reservationStation.executeInstructions();
      this._executionUnits.tick();

      this._instructionsExecuted += this._reorderBuffer.writeBack();
    }
  }

  get clockCycles() { return this._clockCycles; }
  get instructionsExecuted() { return this._instructionsExecuted; }
}
