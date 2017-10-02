
:- module(basics, [
			  empty_lines//0,
			  whites_string_whites//1,
			  codes_type//2,
			  code_type//2,
			  eol//0
	   ]).

:- use_module(library(dcg/basics)).

empty_lines -->
	eos,
	!.

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


whites_string_whites(String) -->
	whites,
	String,
	whites.


codes_type([Code|Codes], Type) -->
	code_type(Code, Type),
	!,
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
