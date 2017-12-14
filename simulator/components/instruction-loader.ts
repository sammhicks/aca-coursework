import { readFileSync } from "fs"

import { Instruction } from "../instructions/instruction";
import { Add } from "../instructions/arithmetic/add";
import { Subtract } from "../instructions/arithmetic/subtract";
import { Multiply } from "../instructions/arithmetic/multiply";
import { Compare } from "../instructions/arithmetic/compare";
import { CompareImmediate } from "../instructions/arithmetic/compare-immediate";

import { Load } from "../instructions/memory/load";
import { Store } from "../instructions/memory/store";

import { Branch } from "../instructions/branch/branch";
import { Jump } from "../instructions/branch/jump";
import { ConditionalJump } from "../instructions/branch/conditional-jump";

import { Return } from "../instructions/branch/return";

import { Random } from "../instructions/io/random";

import { Log } from "../instructions/io/log";
import { Out } from "../instructions/io/out";

import { Halt } from "../instructions/misc/halt";

var pneumonicTable: { [pneumonic: string]: typeof Instruction; } = {};
pneumonicTable[Add.pneumonic] = Add;
pneumonicTable[Subtract.pneumonic] = Subtract;
pneumonicTable[Multiply.pneumonic] = Multiply;
pneumonicTable[Compare.pneumonic] = Compare;
pneumonicTable[CompareImmediate.pneumonic] = CompareImmediate;

pneumonicTable[Load.pneumonic] = Load;
pneumonicTable[Store.pneumonic] = Store;

pneumonicTable[Branch.pneumonic] = Branch;
pneumonicTable[Jump.pneumonic] = Jump;
pneumonicTable[ConditionalJump.pneumonic] = ConditionalJump;

pneumonicTable[Return.pneumonic] = Return;

pneumonicTable[Random.pneumonic] = Random;

pneumonicTable[Log.pneumonic] = Log;
pneumonicTable[Out.pneumonic] = Out;

pneumonicTable[Halt.pneumonic] = Halt;



export function loadInstructions(path: string): Instruction[] {
  const rawInstructions: Instruction[] = JSON.parse(readFileSync(path, 'utf8'));

  var parsedInstructions: Instruction[] = [];

  for (var index = 0; index < rawInstructions.length; index++) {
    const rawInstruction = rawInstructions[index];
    const pneumonic = rawInstruction.name;

    const instructionType = pneumonicTable[pneumonic];

    if (instructionType == undefined) {
      throw Error("Unknown Instruction Pneumonic: " + pneumonic);
    }

    parsedInstructions.push(Object.assign(Object.create(instructionType.prototype), rawInstruction));
  }

  return parsedInstructions;
}

