import { Semaphore } from "../util"

export type Register = number;
export type Literal = number;

export interface RegisterState {
  register: Register;
  value: Literal;
  semaphore: Semaphore;
}
