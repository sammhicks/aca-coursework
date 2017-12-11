/*import * as process from "process"

import { loadInstructions } from "./simulator/components/instruction-loader";
import { InstructionCategories as IC } from "./simulator/instructions/instruction";
import { RegisterFile } from "./simulator/components/register-file";
import { ExecutionUnit } from "./simulator/components/execution-unit";
import { ReorderBuffer } from "./simulator/components/reorder-buffer";

import { initArray } from "./simulator/util";

const sourceFile = process.argv[2];

const instructions = loadInstructions(sourceFile);

const registerFile = new RegisterFile();

const reorderBufferSlotsCount = 3;
const reorderBuffer = new ReorderBuffer(registerFile, reorderBufferSlotsCount);

const arithmeticExecutionUnitCount = 5;
const memoryExecutionUnitCount = 2;
const branchExecutionInstructionCount = 2;
const ioExecutionInstructionCount = 2;
const miscExecutionInstructionCount = 2;

const executionUnits = ([] as ExecutionUnit[]).concat(
  initArray(arithmeticExecutionUnitCount, () => new ExecutionUnit(IC.Arithmetic)),
  initArray(memoryExecutionUnitCount, () => new ExecutionUnit(IC.Memory)),
  initArray(branchExecutionInstructionCount, () => new ExecutionUnit(IC.Branch)),
  initArray(ioExecutionInstructionCount, () => new ExecutionUnit(IC.IO)),
  initArray(miscExecutionInstructionCount, () => new ExecutionUnit(IC.Misc)),
);

var clockCycleCount = 0;
var instructionsExecutedCount = 0;

while (registerFile.running) {
  for (var i = 0; i < executionUnits.length; ++i) {
    const executionUnit = executionUnits[i];
    if (executionUnit.isAvailable) {
      const nextInstruction = instructions[registerFile.pc];

      registerFile.pc = nextInstruction.expectedPC(registerFile.pc);

      executionUnit.executeInstruction(registerFile.lookupInteractions(nextInstruction.requirements), nextInstruction, reorderBuffer.newSlot(executionUnit));
    }
  }

  for (var i = 0; i < executionUnits.length; ++i) { executionUnits[i].tick(); }

  instructionsExecutedCount += reorderBuffer.writeBack();

  ++clockCycleCount;
}

console.log("Instructions Executed:", instructionsExecutedCount);
console.log("Clock Cycles:", clockCycleCount);
console.log("Clock Cycles per Instruction:", (clockCycleCount / instructionsExecutedCount).toFixed(3));
console.log("Instructions per Clock Cycle:", (instructionsExecutedCount / clockCycleCount).toFixed(3));

process.exit();
*/
