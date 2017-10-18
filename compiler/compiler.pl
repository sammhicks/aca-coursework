
:- module(compiler, [
			  compile_all/0,
			  compile_folder/1,
			  compile_file/2
		  ]).

:- use_module(parser/parser).
:- use_module(compiler/compile).
:- use_module(optimiser/optimiser).


compile_all :-
	compile_folder("../feature-tests"),
	compile_folder("../benchmarks").


compile_folder(Folder) :-
	string_concat(Folder, "/*.src", Pattern),
	expand_file_name(Pattern, Files),
	maplist(compile_file, Files).


compile_file(Src) :-
	string_concat(Name, ".src", Src),
	string_concat(Name, ".json", Out),
	compile_file(Src, Out).


compile_file(Src, Out) :-
	phrase_from_file(script(S), Src),
	compile_script(S, Compiled),
	optimise(Compiled, Optimised),
	maplist(opcode, Optimised, Dicts),
	json_value(Dicts, Out_Codes, []),
	string_codes(Out_String, Out_Codes),
	open(Out, write, OutStream),
	write(OutStream, Out_String),
	write(OutStream, "\n"),
	close(OutStream).


opcode(add(R0, R12, I3), op{name:"add", r0:R0, r12:R12, i3:I3}).
opcode(sub(R0, R1, R2, I3), op{name:"sub", r0:R0, r1:R1, r2:R2, i3:I3}).
opcode(mult(R0, R12, I3), op{name:"mult", r0:R0, r12:R12, i3:I3}).
opcode(cmp(R0, R1, R2), op{name:"cmp", r0:R0, r1:R1, r2:R2}).
opcode(cmpi(R0, R1, I2), op{name:"cmpi", r0:R0, r1:R1, i2:I2}).

opcode(ld(R0, R12, I3), op{name:"ld", r0:R0, r12:R12, i3:I3}).
opcode(st(R0, R12, I3), op{name:"st", r0:R0, r12:R12, i3:I3}).

opcode(b(I0), op{name: "b", i0:I0}).
opcode(j(I0), op{name: "j", i0:I0}).
opcode(cj(I0, Inv, Cond, R1), op{name:"cj", i0:I0, inv:Inv, cond:Cond, r1:R1}).

opcode(ret, op{name:"ret"}).

opcode(rand(R0, A1, B2), op{name:"rand", r0:R0, a1:A1, b2:B2}).

opcode(log(Message), op{name:"log", message:Message}).
opcode(out(Label, R0), op{name:"out", label:Label, r0:R0}).

opcode(noop, op{name:"noop"}).
opcode(halt, op{name:"halt"}).


json_value(null) -->
	"null".

json_value(true) -->
	"true".

json_value(false) -->
	"false".

json_value(Object, Codes, Tail) :-
	is_dict(Object),
	!,
	json_object(Object, Codes, Tail).

json_value([]) -->
	"[]".

json_value(Array, Codes, Tail) :-
	is_list(Array),
	!,
	json_array(Array, Codes, Tail).

json_value(Int, Codes, Tail) :-
	integer(Int),
	!,
	json_int(Int, Codes, Tail).

json_value(String, Codes, Tail) :-
	string(String),
	!,
	json_string(String, Codes, Tail).


json_object(Object, Codes, Tail) :-
	dict_pairs(Object, _, Pairs),
	json_braced_pairs(Pairs, Codes, Tail).


json_braced_pairs(Pairs) -->
	"{",
	json_pairs(Pairs),
	"}".


json_pairs([Pair]) -->
	!,
	json_pair(Pair).

json_pairs([Pair|Pairs]) -->
	json_pair(Pair),
	!,
	",",
	json_pairs(Pairs).


json_pair(Name-Value) -->
	json_atom(Name),
	":",
	json_value(Value).


json_array(Items) -->
	"[",
	json_array_values(Items),
	"]".


json_array_values([Value]) -->
	!,
	json_value(Value).

json_array_values([Value|Values]) -->
	json_value(Value),
	!,
	",",
	json_array_values(Values).


json_int(Int, Codes, Tail) :-
	format(codes(Codes, Tail), "~w", [Int]).


json_atom(Atom, Codes, Tail) :-
	atom_chars(Atom, Chars),
	json_quoted_chars(Chars, Codes, Tail).


json_string(String, Codes, Tail) :-
	string_chars(String, Chars),
	json_quoted_chars(Chars, Codes, Tail).


json_quoted_chars(Chars) -->
	"\"",
	json_chars(Chars),
	"\"".


json_chars([]) -->
	[].

json_chars([Char|Chars]) -->
	json_char(Char),
	!,
	json_chars(Chars).


json_char('"') -->
	"\\\"".

json_char('\\') -->
	"\\\\".

json_char('/') -->
	"/".

json_char('\b') -->
	"\\b".

json_char('\f') -->
	"\\f".

json_char('\n') -->
	"\\n".

json_char('\r') -->
	"\\r".

json_char('\t') -->
	"\\r".

json_char(Char, [Code|Tail], Tail) :-
	char_code(Char, Code).
