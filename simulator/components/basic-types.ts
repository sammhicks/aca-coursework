export type Address = number;
export type PC = number;
export type Register = number;
export type Literal = number;

export interface Registers {
  [index: number]: Literal;
}

export interface Memory {
  [index: number]: Literal;
}
