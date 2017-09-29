
:- module(parser, [
              script//1
          ]).

:- use_module(library(dcg/basics)).

script(script(Instructions, Functions)) -->
    empty_lines,
    whites,
    instructions(Instructions),
    empty_lines,
    function_declarations(Functions),
    empty_lines,
    eos.


function_declarations([]) -->
    [].

function_declarations([F]) -->
    function_declaration(F).

function_declarations([F|Fs]) -->
    function_declaration(F),
    function_declarations_tail(Fs).


function_declarations_tail([F|Fs]) -->
    empty_lines,
    function_declaration(F),
    function_declarations_tail(Fs).

function_declarations_tail([]) -->
    [].


function_declaration(function(Name, Arguments, Body)) -->
    variable(Name),
    whites_string_whites("("),
    variable_list(Arguments),
    whites_string_whites(")"),
    whites,
    "{",
    empty_lines,
    instructions(Body),
    empty_lines,
    "}".


instructions([Instruction|Instructions]) -->
    instruction_with_eol(Instruction),
    instructions_tail(Instructions).

instructions([Instruction]) -->
    instruction_with_eol(Instruction).

instructions([]) -->
    [].


instructions_tail([Instruction|Instructions]) -->
    empty_lines,
    instruction_with_eol(Instruction),
    instructions_tail(Instructions).

instructions_tail([]) -->
    [].


instruction_with_eol(Instruction) -->
    whites,
    instruction(Instruction),
    whites,
    eol,
    !.

instruction(var(Name)) -->
    "var",
    white,
    whites,
    variable(Name).

instruction(var(Name, Size)) -->
    "var",
    white,
    whites,
    named_braced_integer(Name, Size).

instruction(add(R, A, B)) -->
    binop("+", R, A, B).

instruction(sub(R, A, B)) -->
    binop("-", R, A, B).

instruction(mult(R, A, B)) -->
    binop("*", R, A, B).

instruction(read(R, Array, Index)) -->
    variable(R),
    whites_string_whites("="),
    named_braced_integer(Array, Index).

instruction(write(R, Array, Index)) -->
    named_braced_integer(Array, Index),
    whites_string_whites("="),
    variable(R).

instruction(call(Function, [Arguments])) -->
    variable(Function),
    whites_string_whites("("),
    variable_or_number_list(Arguments),
    whites_string_whites(")").


empty_lines -->
    [].

empty_lines -->
    empty_line,
    empty_lines.


empty_line -->
    whites,
    eol.

empty_line -->
    whites,
    "//",
    string_without("\r\n", _),
    eol.


named_braced_integer(V, N) -->
    variable(V),
    whites,
    braced_integer(N).


braced_integer(N) -->
    "[",
    whites,
    integer(N),
    whites,
    "]".


binop(Op, R, A, B) -->
    variable(R),
    whites_string_whites("="),
    variable_or_number(A),
    whites_string_whites(Op),
    variable_or_number(B).


variable_or_number_list([]) -->
    [].

variable_or_number_list([Item]) -->
    variable_or_number(Item).

variable_or_number_list([Item|Items]) -->
    variable_or_number(Item),
    variable_or_number_tail(Items).


variable_or_number_tail([Item|Items]) -->
    whites_string_whites(","),
    !,
    variable_or_number(Item),
    variable_or_number_tail(Items).

variable_or_number_tail([]) -->
    [].


variable_list([]) -->
    [].

variable_list([V]) -->
    variable(V).

variable_list([V|Vs]) -->
    variable(V),
    variable_list_tail(Vs).


variable_list_tail([]) -->
    [].

variable_list_tail([V|Vs]) -->
    whites_string_whites(","),
    variable(V),
    variable_list_tail(Vs).


variable_or_number(v(V)) -->
    variable(V).

variable_or_number(n(N)) -->
    number(N).


variable(Variable) -->
    code_type(First, alpha),
    codes_type(Other, alnum),
    {
        string_codes(Variable, [First|Other])
    }.


whites_string_whites(String) -->
    whites,
    String,
    whites.


codes_type([Code|Codes], Type) -->
    code_type(Code, Type),
    codes_type(Codes, Type).

codes_type([], _) -->
    [].


code_type(Code, Type) -->
    [Code],
    {
        char_type(Code, Type)
    }.

eol -->
    "\r\n",
    !.

eol -->
    "\r".

eol -->
    "\n".
