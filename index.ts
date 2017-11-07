import * as process from "process"

import { loadInstructions } from "./simulator/components/instruction-loader";
import { RegisterFile } from "./simulator/components/register-file";
import { ExecutionUnit } from "./simulator/components/execution-unit";
import { ReorderBuffer } from "./simulator/components/reorder-buffer";

import { initArray } from "./simulator/util";

const sourceFile = process.argv[2];

const instructions = loadInstructions(sourceFile);

const registerFile = new RegisterFile();

const executionUnitCount = 1;
const executionUnits = initArray(executionUnitCount, () => new ExecutionUnit());

const reorderBufferSlotsCount = 3;
const reorderBuffer = new ReorderBuffer(registerFile, reorderBufferSlotsCount);

var clockCycleCount = 0;
var instructionsExecutedCount = 0;

while (registerFile.running) {
  for (var i = 0; i < executionUnitCount; ++i) {
    const executionUnit = executionUnits[i];
    if (executionUnit.isAvailable) {
      const nextInstruction = instructions[registerFile.pc];

      registerFile.pc = nextInstruction.expectedPC(registerFile.pc);

      executionUnit.executeInstruction(registerFile.lookupInteractions(nextInstruction.requirements), nextInstruction, reorderBuffer.newSlot(executionUnit));
    }
  }

  for (var i = 0; i < executionUnitCount; ++i) {
    const executionUnit = executionUnits[i];
    executionUnit.tick();
  }

  instructionsExecutedCount += reorderBuffer.writeBack();

  ++clockCycleCount;
}

console.log("Instructions Executed:", instructionsExecutedCount);
console.log("Clock Cycles:", clockCycleCount);
console.log("Clock Cycles per Instruction:", (clockCycleCount / instructionsExecutedCount).toFixed(3));
console.log("Instructions per Clock Cycle:", (instructionsExecutedCount / clockCycleCount).toFixed(3));

process.exit();
