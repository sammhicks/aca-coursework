import * as process from "process"

import { Literal, PC, Address } from "./simulator/components/basic-types";
import { loadInstructions } from "./simulator/components/instruction-loader";
import { Processor } from "./simulator/components/processor";
import { thousandsMarker } from "./simulator/util/thousands-marker";

const sourceFile = process.argv[2];

const instructions = loadInstructions(sourceFile);

const reservationStationCount = 8;
const memorySlotCount = 8;

const arithmeticExecutionUnitCount = 2;
const memoryExecutionUnitCount = 4;
const ioExecutionUnitCount = 2;

const processor = new Processor(reservationStationCount, memorySlotCount, arithmeticExecutionUnitCount, memoryExecutionUnitCount, ioExecutionUnitCount, instructions);

processor.run();

const precision = 3;


console.log("Instructions Executed:", thousandsMarker(processor.instructionsExecuted));
console.log("Clock Cycles:", thousandsMarker(processor.parallelClockCycles));
console.log("Clock Cycles per Instruction:", (processor.parallelClockCycles / processor.instructionsExecuted).toFixed(precision));
console.log("Instructions per Clock Cycle:", (processor.instructionsExecuted / processor.parallelClockCycles).toFixed(precision));

console.log("");

console.log("Clock cycles if no pipeline:", thousandsMarker(processor.serialClockCycles));
console.log("Pipeline speedup:", (processor.serialClockCycles / processor.parallelClockCycles).toFixed(precision));

console.log("");

console.log("Branch Prediction: Efficiency:", thousandsMarker(processor.branchPredictionSuccesses), ",", thousandsMarker(processor.branchPredictionFailures), "->", processor.branchPredictionEfficiency.toFixed(3));
console.log("Return Prediction: Efficiency:", thousandsMarker(processor.returnPredictionSuccesses), ",", thousandsMarker(processor.returnPredictionFailures), "->", processor.returnPredictionEfficiency.toFixed(3));

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
