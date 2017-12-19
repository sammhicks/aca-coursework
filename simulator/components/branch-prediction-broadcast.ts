import { PC } from "./basic-types";
import { HandlesBranchPredictionError } from "./register-file";


export class BranchPredictionBroadcaster implements HandlesBranchPredictionError {
  private _listeners: HandlesBranchPredictionError[]

  constructor() {
    this._listeners = [];
  }

  addListeners(...listeners: HandlesBranchPredictionError[]) {
    this._listeners.push(...listeners);
  }

  handleBranchPredictionError(pc: PC) {
    for (let index = 0; index < this._listeners.length; index++) {
      this._listeners[index].handleBranchPredictionError(pc);
    }
  }
}
