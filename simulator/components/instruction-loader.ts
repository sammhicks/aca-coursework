import { readFileSync } from "fs"

import { Instruction } from "../instructions/instruction";
import { Add } from "../instructions/add";
import { Subtract } from "../instructions/subtract";
import { Multiply } from "../instructions/multiply";
import { Compare } from "../instructions/compare";
import { CompareImmediate } from "../instructions/compare-immediate";

import { Load } from "../instructions/load";
import { Store } from "../instructions/store";

import { Branch } from "../instructions/branch";
import { Jump } from "../instructions/jump";
import { ConditionalJump } from "../instructions/conditional-jump";

import { Return } from "../instructions/return";

import { Random } from "../instructions/random";

import { Log } from "../instructions/log";
import { Out } from "../instructions/out";

import { NoOp } from "../instructions/noop";
import { Halt } from "../instructions/halt";

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

pneumonicTable[NoOp.pneumonic] = NoOp;
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

