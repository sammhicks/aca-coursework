import { PC } from "./basic-types";
import { ReservationStation } from "./out-of-order";
import { RegisterFile } from "./register-file";
import { InstructionFetcher } from "./instruction-fetcher";
import { Instruction } from "../instructions/instruction";
import { ExecutionUnits } from "./execution-unit";
import { ReorderBuffer } from "./reorder-buffer";
import { Prediction } from "./prediction";


export class Processor extends RegisterFile {
  private _fetcher: InstructionFetcher;
  private _prediction: Prediction;
  private _reservationStation: ReservationStation;
  private _executionUnits: ExecutionUnits;
  private _reorderBuffer: ReorderBuffer;

  private _running: boolean
  private _serialClockCycles: number
  private _parallelClockCycles: number;
  private _instructionsExecuted: number;

  private _branchPredictionSuccesses: number;
  private _branchPredictionFailures: number;
  private _returnPredictionSuccesses: number;
  private _returnPredictionFailures: number;

  constructor(reservationStationCount: number, memorySlotCount: number, arithmeticExecutionUnitCount: number, memoryExecutionUnitCount: number, ioExecutioUnitCount: number, instructions: Instruction[]) {
    super();

    this._prediction = new Prediction();

    this._fetcher = new InstructionFetcher(instructions, this._prediction)

    this._executionUnits = new ExecutionUnits(arithmeticExecutionUnitCount, memoryExecutionUnitCount, ioExecutioUnitCount, this._prediction);

    this._reorderBuffer = new ReorderBuffer(this);

    this._reservationStation = new ReservationStation(reservationStationCount, memorySlotCount, this._fetcher, this._prediction, this._executionUnits, this._reorderBuffer);

    this._running = true;
    this._serialClockCycles = 0;
    this._parallelClockCycles = 0;
    this._instructionsExecuted = 0;

    this._branchPredictionSuccesses = 0;
    this._branchPredictionFailures = 0;
    this._returnPredictionSuccesses = 0;
    this._returnPredictionFailures = 0;
  }

  releaseRegister() { }

  releaseMemory() { }

  performExternalAction(action: () => void) { action(); }

  get isRunning() { return this._running; }

  halt() {
    this._running = false;
  }

  notifyBranchTaken(pc: PC, branchTaken: boolean) {
    this._prediction.branchPrediction.updateValue(pc, branchTaken);
  }

  handleBranchPredictionSuccess() { this._branchPredictionSuccesses += 1; }

  handleBranchPredictionError(pc: PC) {
    this._branchPredictionFailures += 1;
    this.handlePredictionError(pc);
  }

  notifyReturn(pc: PC, ret: PC) {
    this._prediction.returnPrediction.updateValue(pc, ret);
  }

  handleReturnPredictionSuccess() { this._returnPredictionSuccesses += 1; }

  handleReturnPredictionError(pc: PC) {
    this._returnPredictionFailures += 1;
    this.handlePredictionError(pc);
  }

  private handlePredictionError(pc: PC) {
    this._fetcher.reset(pc);
    this._reservationStation.reset(this._registers, this._memory);
    this._executionUnits.resetUnits();
    this._reorderBuffer.reset();
  }

  run() {
    while (this.isRunning) {
      this._reservationStation.loadInstructions();
      this._reservationStation.executeInstructions();
      this._executionUnits.tick();

      const instructionsExecutedAndDuration = this._reorderBuffer.writeBack();

      this._serialClockCycles += instructionsExecutedAndDuration[1];
      this._parallelClockCycles += 1;
      this._instructionsExecuted += instructionsExecutedAndDuration[0];
    }
  }

  get serialClockCycles() { return this._serialClockCycles; }
  get parallelClockCycles() { return this._parallelClockCycles; }
  get instructionsExecuted() { return this._instructionsExecuted; }

  get branchPredictionSuccesses() { return this._branchPredictionSuccesses; }
  get branchPredictionFailures() { return this._branchPredictionFailures; }
  get branchPredictionEfficiency() { return this.branchPredictionSuccesses / (this.branchPredictionSuccesses + this.branchPredictionFailures); }

  get returnPredictionSuccesses() { return this._returnPredictionSuccesses; }
  get returnPredictionFailures() { return this._returnPredictionFailures; }
  get returnPredictionEfficiency() { return this.returnPredictionSuccesses / (this.returnPredictionSuccesses + this.returnPredictionFailures); }

  get registers() { return this._registers.slice(); }
  get memory() { return this._memory.slice(); }
}
