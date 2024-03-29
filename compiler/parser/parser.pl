
:- module(parser, [
              script//1     % -Script
          ]).

:- use_module(library(dcg/basics)).

:- use_module(assignments).
:- use_module(basics).
:- use_module(identifiers).

script(script(Instructions, Functions)) -->
	empty_lines,
	whites,
	instructions(Main),
	{
		append(Main, [halt], Instructions)
	},
	empty_lines,
	function_declarations(Functions),
	empty_lines,
	eos.


function_declarations([]) -->
	eos,
	!.

function_declarations([F|Fs]) -->
	function_declaration_with_eol(F),
	!,
	function_declarations_tail(Fs).

function_declarations([]) -->
	[].


function_declarations_tail([]) -->
	eos,
	!.

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


function_declaration(function(Name, Returns, Arguments, Body)) -->
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


instructions([]) -->
	eos,
	!.

instructions([Instruction|Instructions]) -->
	instruction_with_eol(Instruction),
	!,
	instructions_tail(Instructions).

instructions([]) -->
	[].


instructions_tail([]) -->
	eos,
	!.

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

instruction(Assignment) -->
	assignment(Assignment).

instruction(array(Name, Length)) -->
	var_keyword,
	whites,
	array_with_index(Name, n(Length)).

instruction(var(Name)) -->
	var_keyword,
	whites,
	variable(Name).

instruction(call(Function, Returns, Arguments)) -->
	variable(Function),
	whites_string_whites("("),
	variable_or_number_list(Arguments),
	whites_string_whites(")"),
	returns(Returns).

instruction(outs(String)) -->
	log_keyword,
	whites,
	string_without("\r\n", Codes),
	{
		string_codes(String, Codes)
	}.

instruction(outv(Variable)) -->
	out_keyword,
	whites,
	variable(Variable).

instruction(If) -->
	if(If).

instruction(for(Setup, Condition, Increment, Do)) -->
	for_keyword,
	whites,
	for_instructions(Setup),
	whites_string_whites(";"),
	condition(Condition),
	whites_string_whites(";"),
	for_instructions(Increment),
	whites,
	body(Do).

instruction(for(Condition, Do)) -->
	for_keyword,
	whites,
	condition(Condition),
	whites,
	body(Do).


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


comparison(<=) -->
	"<=",
	!.

comparison(>=) -->
	">=",
	!.

comparison(<>) -->
	"!=",
	!.

comparison(<) -->
	"<",
	!.

comparison(>) -->
	">",
	!.

comparison(=) -->
	"=",
	!.


if(if(Condition, Then, Else)) -->
	"if",
	whites,
	condition(Condition),
	whites,
	body(Then),
	else(Else).


else([If]) -->
	whites,
	"else",
	white,
	whites,
	if(If),
	!.

else(Else) -->
	whites,
	"else",
	white,
	whites,
	body(Else),
	!.

else(noop) -->
	[].


precondition([Instruction|Instructions]) -->
	instruction(Instruction),
	whites_string_whites(";"),
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
