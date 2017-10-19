import * as process from "process"

import { loadInstructions } from "./simulator/components/instruction-loader";
import { RegisterFile } from "./simulator/components/register-file";

const sourceFile = process.argv[2];

const instructions = loadInstructions(sourceFile);

var registerFile = new RegisterFile();

var clockCycleCount = 0;
var instructionsExecutedCount = 0;

while (registerFile.running) {
  const nextInstruction = instructions[registerFile.pc];

  registerFile.pc += 1;

  clockCycleCount += nextInstruction.duration();
  instructionsExecutedCount += 1;

  const writes = nextInstruction.execute(registerFile);

  for (var index = 0; index < writes.length; index++) {
    writes[index].write(registerFile);
  }
}

console.log("Instructions Executed:", instructionsExecutedCount);
console.log("Clock Cycles:", clockCycleCount);
console.log("Instructions per Clock Cycle:", instructionsExecutedCount / clockCycleCount);

process.exit();
