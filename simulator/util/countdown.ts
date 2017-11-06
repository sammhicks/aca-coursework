import { Abortable } from "./abortable";
import { ClockDriven } from "./clock-driven";

export abstract class Countdown implements Abortable, ClockDriven {
  private _remaining: number;

  constructor() {
    this._remaining = 0;
  }

  protected reset(duration: number): void {
    this._remaining = duration;
  }

  protected abstract onCompletion(): void;

  protected abstract onAborted(): void;


  public get isCompleted() {
    return this._remaining == 0;
  }

  public tick(): void {
    if (this.isCompleted) {
      return;
    }

    --this._remaining;

    if (this.isCompleted) {
      this.onCompletion();
    }
  }

  public abort(): void {
    this._remaining = 0;

    this.onAborted();
  }
}
