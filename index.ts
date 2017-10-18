import * as process from "process"

import { loadInstructions } from "./simulator/components/instruction-loader";
import { RegisterFile } from "./simulator/components/register-file";

const sourceFile = process.argv[2];

const instructions = loadInstructions(sourceFile);

var registerFile = new RegisterFile();

while (registerFile.running) {
  const nextInstruction = instructions[registerFile.pc];

  if (!nextInstruction.effects().pc) {
    registerFile.pc += 1;
  }

  const writes = nextInstruction.execute(registerFile);

  for (var index = 0; index < writes.length; index++) {
    writes[index].write(registerFile);
  }
}

process.exit();
