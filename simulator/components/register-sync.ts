import { DeepCopy, deepCopyArray } from "../util/deep-copy";
import { Semaphore } from "../util/semaphore";

export class RegisterSync implements DeepCopy<RegisterSync> {
  constructor(public registers: Semaphore[], public memory: Semaphore[], public branch: Semaphore) { }

  deepCopy() {
    return new RegisterSync(deepCopyArray(this.registers), deepCopyArray(this.memory), this.branch.deepCopy())
  }
};
