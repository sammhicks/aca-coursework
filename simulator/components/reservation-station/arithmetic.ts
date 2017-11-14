import { InstructionInteractions } from "./instruction-interactions";
import { LikeRegisterFile } from "./register-file";
import { RegisterSync } from "./register-sync";

interface ReservationStationState { }

class IdleReservationStation implements ReservationStationState { }

class ActiveReservationStation implements ReservationStationState {
  private _sync: RegisterSync;
  private _remainingInteractions: InstructionInteractions;
  private _instructionArguments: RegisterSync;

  constructor(currentSync: RegisterSync, instructionSync: RegisterSync) {
  }

  update
}

export class ReservationStation {


  constructor
}
