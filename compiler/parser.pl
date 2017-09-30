
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


function_declarations([F|Fs]) -->
	function_declaration_with_eol(F),
	!,
	function_declarations_tail(Fs).

function_declarations([]) -->
	[].


function_declarations_tail([F|Fs]) -->
	empty_lines,
	function_declaration_with_eol(F),
	!,
	function_declarations_tail(Fs).

function_declarations_tail([]) -->
	[].


function_declaration_with_eol(Function) -->
	whites,
	function_declaration(Function),
	whites,
	eol,
	!.


function_declaration(function(Name, Arguments, Returns, Body)) -->
	variable(Name),
	whites_string_whites("("),
	variable_list(Arguments),
	whites_string_whites(")"),
	returns(Returns),
	whites,
	"{",
	empty_lines,
	instructions(Body),
	empty_lines,
	"}".


instructions([Instruction|Instructions]) -->
	instruction_with_eol(Instruction),
	!,
	instructions_tail(Instructions).

instructions([]) -->
	[].


instructions_tail([Instruction|Instructions]) -->
	empty_lines,
	instruction_with_eol(Instruction),
	!,
	instructions_tail(Instructions).

instructions_tail([]) -->
	[].


instruction_with_eol(Instruction) -->
	whites,
	instruction(Instruction),
	whites,
	eol,
	!.

instruction(noop) -->
	"noop".

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

instruction(ass(R, A)) -->
	variable(R),
	whites_string_whites("="),
	variable_or_number(A).

instruction(add(R, A, B)) -->
	binop("+", R, A, B).

instruction(sub(R, A, B)) -->
	binop("-", R, A, B).

instruction(mult(R, A, B)) -->
	binop("*", R, A, B).

instruction(cmp(R, A, B)) -->
	binop("<>", R, A, B).

instruction(read(R, Array, Index)) -->
	variable(R),
	whites_string_whites("="),
	named_braced_integer(Array, Index).

instruction(write(R, Array, Index)) -->
	named_braced_integer(Array, Index),
	whites_string_whites("="),
	variable(R).

instruction(call(Function, Arguments, Returns)) -->
	variable(Function),
	whites_string_whites("("),
	variable_or_number_list(Arguments),
	whites_string_whites(")"),
	returns(Returns).

instruction(return) -->
	"return".

instruction(if(If, Then, Else)) -->
	"if",
	whites,
	variable(If),
	body(Then),
	whites_string_whites("else"),
	body(Else).

instruction(if(Condition, Then)) -->
	"if",
	whites,
	condition(Condition),
	whites,
	body(Then).

instruction(for(Setup, Condition, Increment, Do)) -->
	for_header,
	for_instructions(Setup),
	whites_string_whites(";"),
	condition(Condition),
	whites_string_whites(";"),
	for_instructions(Increment),
	whites,
	body(Do).

instruction(for(Condition, Do)) -->
	for_header,
	condition(Condition),
	whites,
	body(Do).

instruction(break) -->
	"break".


for_header -->
	"for",
	whites.


condition(cond(Precondition, Variable, Comparison)) -->
	"{",
	whites,
	precondition(Precondition),
	whites,
	variable_comparision(Variable, Comparison),
	whites,
	"}".

condition(cond(Variable, Comparison)) -->
	variable_comparision(Variable, Comparison).


variable_comparision(Variable, Comparison) -->
	variable(Variable),
	whites,
	comparison(Comparison).


comparison(<) -->
	"<",
	!.

comparison(>) -->
	">",
	!.

comparison(=) -->
	"=",
	!.


precondition([Instruction|Instructions]) -->
	instruction(Instruction),
	whites,
	";",
	!,
	precondition(Instructions).

precondition([]) -->
	[].


for_instructions([Instruction|Instructions]) -->
	"{",
	whites,
	instruction(Instruction),
	for_instructions_tail(Instructions),
	whites,
	"}".

for_instructions([Instruction]) -->
	instruction(Instruction).


for_instructions_tail([Instruction|Instructions]) -->
	";",
	whites,
	instruction(Instruction),
	!,
	for_instructions_tail(Instructions).


for_instructions_tail([]) -->
	[].


body(Body) -->
	"{",
	whites,
	eol,
	instructions(Body),
	whites,
	"}".

returns(Returns) -->
	"->",
	whites,
	"(",
	whites,
	variable_list(Returns),
	whites,
	")".

returns([Variable]) -->
	"->",
	whites,
	variable(Variable).

returns([]) -->
	[].


empty_lines -->
	empty_line,
	!,
	empty_lines.

empty_lines -->
	[].


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


variable_or_number_list([Item|Items]) -->
	variable_or_number(Item),
	variable_or_number_tail(Items).

variable_or_number_list([]) -->
	[].


variable_or_number_tail([Item|Items]) -->
	whites_string_whites(","),
	!,
	variable_or_number(Item),
	variable_or_number_tail(Items).

variable_or_number_tail([]) -->
	[].


variable_list([V|Vs]) -->
	variable(V),
	variable_list_tail(Vs).

variable_list([]) -->
	[].


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

