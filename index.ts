import * as process from "process"

import { Literal, PC, Address } from "./simulator/components/basic-types";
import { loadInstructions } from "./simulator/components/instruction-loader";
import { Processor } from "./simulator/components/processor";

const sourceFile = process.argv[2];

const instructions = loadInstructions(sourceFile);

const reservationStationCount = 8;
const memorySlotCount = 8;

const arithmeticExecutionUnitCount = 4;
const memoryExecutionUnitCount = 2;
const ioExecutionUnitCount = 2;

const processor = new Processor(reservationStationCount, memorySlotCount, arithmeticExecutionUnitCount, memoryExecutionUnitCount, ioExecutionUnitCount, instructions);

processor.run();

const precision = 3;


console.log("Instructions Executed:", processor.instructionsExecuted);
console.log("Clock Cycles:", processor.parallelClockCycles);
console.log("Clock Cycles per Instruction:", (processor.parallelClockCycles / processor.instructionsExecuted).toPrecision(precision));
console.log("Instructions per Clock Cycle:", (processor.instructionsExecuted / processor.parallelClockCycles).toPrecision(precision));

console.log("");

console.log("Clock cycles if no pipeline:", processor.serialClockCycles);
console.log("Pipeline speedup:", (processor.serialClockCycles / processor.parallelClockCycles).toPrecision(precision))

console.log("");

console.log("Branch Prediction: Efficiency:", processor.branchPredictionSuccesses, ",", processor.branchPredictionFailures, "->", processor.branchPredictionEfficiency.toPrecision(3));
console.log("Return Prediction: Efficiency:", processor.returnPredictionSuccesses, ",", processor.returnPredictionFailures, "->", processor.returnPredictionEfficiency.toPrecision(3));

console.log("");

console.log("Registers:")
processor.registers.forEach(function (value: Literal, pc: PC) {
  console.log("\t", pc, "-", value);
})

console.log("");

console.log("Memory:")
processor.memory.forEach(function (value: Literal, addr: Address) {
  console.log("\t", addr, "-", value);
})

process.exit();
