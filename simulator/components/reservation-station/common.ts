import { LikeRegisterFile } from "../register-file";

export interface ReservationStationState { }

export class RegisterDependencies {
  private readonly _rf: LikeRegisterFile;

  constructor(rf: LikeRegisterFile) {
    this._rf = rf;
  }


}
