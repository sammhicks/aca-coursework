import { ClockDriven } from "./clock-driven";

export abstract class Countdown extends ClockDriven {
  private remaining: number;

  constructor() {
    super();
    this.remaining = 0;
  }

  protected reset(duration: number): void {
    this.remaining = duration;
  }

  protected abstract onCompletion(): void;


  public completed() {
    return this.remaining == 0;
  }

  public tick(): void {
    if (this.completed()) {
      return;
    }

    --this.remaining;

    if (this.completed()) {
      this.onCompletion();
    }
  }
}
