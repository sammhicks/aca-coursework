import * as process from "process"

import { loadInstructions } from "./simulator/components/instruction-loader";
import { Processor } from "./simulator/components/processor";

const sourceFile = process.argv[2];

const instructions = loadInstructions(sourceFile);

const reservationStationCount = 8;
const memorySlotCount = 8;

const arithmeticExecutionUnitCount = 4;
const memoryExecutionUnitCount = 2;

const processor = new Processor(reservationStationCount, memorySlotCount, arithmeticExecutionUnitCount, memoryExecutionUnitCount, instructions);

processor.run();

console.log("Instructions Executed:", processor.instructionsExecuted);
console.log("Clock Cycles:", processor.clockCycles);
console.log("Clock Cycles per Instruction:", (processor.clockCycles / processor.instructionsExecuted).toPrecision(3));
console.log("Instructions per Clock Cycle:", (processor.instructionsExecuted / processor.clockCycles).toPrecision(3));

process.exit();
