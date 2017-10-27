import * as process from "process"

import { loadInstructions } from "./simulator/components/instruction-loader";
import { RegisterFile } from "./simulator/components/register-file";
import { ExecutionUnit } from "./simulator/components/execution-unit";

const sourceFile = process.argv[2];

const instructions = loadInstructions(sourceFile);

var registerFile = new RegisterFile();

var executionUnit = new ExecutionUnit(registerFile);

var clockCycleCount = 0;
var instructionsExecutedCount = 0;

while (registerFile.running) {
  const nextInstruction = instructions[registerFile.pc];

  registerFile.pc += 1;

  executionUnit.executeInstruction(nextInstruction);

  while (!executionUnit.completed()) {
    ++clockCycleCount;
    executionUnit.tick();
  }

  instructionsExecutedCount += 1;
}

console.log("Instructions Executed:", instructionsExecutedCount);
console.log("Clock Cycles:", clockCycleCount);
console.log("Clock Cycles per Instruction:", (clockCycleCount / instructionsExecutedCount).toFixed(3));
console.log("Instructions per Clock Cycle:", (instructionsExecutedCount / clockCycleCount).toFixed(3));

process.exit();
