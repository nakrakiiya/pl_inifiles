% -*- coding: utf-8 -*-
:- module(stringutils, [
                          string_ltrim/2,
                          string_rtrim/2,
                          string_trim/2,
                          utf8_byte_seq_to_codes/2,
                          utf8_byte_seq_to_atom/2,
                          upside_down_text_gen/2
                       ]).

:- mode utf8_byte_seq_to_codes(+, -),
        utf8_byte_seq_to_atom(+, -),
        string_trim(+, -),
        string_ltrim(+, -),
        string_rtrim(+, -),
        upside_down_text_gen(+, -).

whitespace(32).
whitespace(10).
whitespace(13).
whitespace(9). % TAB

% string_ltrim(+Codes, -Result)
string_ltrim(String, Result) :-
% in SICSTUS, the solutions' order are always as below
%        | ?- append(A, B, [1,2,3]).
%        A = [],
%        B = [1,2,3] ? ;
%        A = [1],
%        B = [2,3] ? ;
%        A = [1,2],
%        B = [3] ? ;
%        A = [1,2,3],
%        B = [] ? ;
%        no
        append(_, Rest, String), 
        ( Rest = [] -> % all are whitespaces
          Result = []
        ; ( Rest = [A|_],
            whitespace(A)
          -> 
            fail % force to backtrack
          ;
            !,
            Result = Rest )).

% string_rtrim(+Codes, -Result)
string_rtrim([],[],false) :- !.
string_rtrim([A|Rest], Result, IsCanUseTheCode) :-
        string_rtrim(Rest, Result1, IsCanUseTheCode1),
        ( IsCanUseTheCode1 ->
          Result = [A|Result1],
          IsCanUseTheCode = IsCanUseTheCode1
        ; 
          ( whitespace(A) ->
            Result = Result1,
            IsCanUseTheCode = false
          ; 
            Result = [A|Result1],
            IsCanUseTheCode = true )).
string_rtrim(String, Result) :-
        string_rtrim(String, Result, _).

% string_trim(+Codes, -Result)
string_trim(String, Result) :-
        string_ltrim(String, R0),
        string_rtrim(R0, Result).
        

% convert utf8 byte sequence list to codes
% example:
% utf8_byte_seq_to_codes([0xE4, 0xBD, 0xA0, 0xE5, 0xA5, 0xBD, 0x20, 0x20, 0x61, 0x73, 0x64, 0x66, 0x20, 0x20, 0xE5, 0x90, 0x97, 0xEF, 0xBC, 0x9F], X), atom_codes(A, X).
% A = '你好  asdf  吗？',
% X = [20320,22909,32,32,97,115,100,102,32,32|...] ? ;
% no
utf8_byte_seq_to_codes([], []) :- !.
utf8_byte_seq_to_codes([H1,H2,H3,H4|T], [X|R]) :- % 4 bytes character        
        H1 - (H1 /\ 0b111) =:= 0b11110000,  
        H2 - (H2 /\ 0b111111) =:= 0b10000000,
        H3 - (H3 /\ 0b111111) =:= 0b10000000,
        H4 - (H4 /\ 0b111111) =:= 0b10000000,
        !,
        X is (H1 /\ 0b111) * 262144 + (H2 /\ 0b111111) * 4096 + (H3 /\ 0b111111) * 64 + (H4 /\ 0b111111),
        utf8_byte_seq_to_codes(T, R).
utf8_byte_seq_to_codes([H1,H2,H3|T], [X|R]) :- % 3 bytes character        
        H1 - (H1 /\ 0b1111) =:= 0b11100000,  
        H2 - (H2 /\ 0b111111) =:= 0b10000000,
        H3 - (H3 /\ 0b111111) =:= 0b10000000,
        !,
        X is (H1 /\ 0b1111) * 4096 + (H2 /\ 0b111111) * 64 + (H3 /\ 0b111111),
        utf8_byte_seq_to_codes(T, R).
utf8_byte_seq_to_codes([H1,H2|T], [X|R]) :- % 2 bytes character        
        H1 - (H1 /\ 0b11111) =:= 0b11000000,  
        H2 - (H2 /\ 0b111111) =:= 0b10000000,
        !,
        X is (H1 /\ 0b11111) * 64 +  (H2 /\ 0b111111),
        utf8_byte_seq_to_codes(T, R).
utf8_byte_seq_to_codes([H1|T], [X|R]) :- % 1 byte character        
        H1 /\ 0b1111111 =:= H1,
        !,
        X is H1 /\ 0b1111111,
        utf8_byte_seq_to_codes(T, R).
utf8_byte_seq_to_codes([_|T], [63|R]) :- % error, skip and use '?' instead        
        utf8_byte_seq_to_codes(T, R).
utf8_byte_seq_to_atom(ByteSeq, Atom) :-
        utf8_byte_seq_to_codes(ByteSeq, Codes),
        atom_codes(Atom, Codes).






% to_lowercase(+In, -Out)
% In, Out are the codes of the the char
to_lowercase(In, Out) :-
        In >= 0'A,
        In =< 0'Z,
        Out is In - 0'A + 0'a, 
        !.
to_lowercase(X, X).


% upside_down_text_gen(+Text, -Result)
% 将Text的内容上下颠倒...暂时还没有支持大写字母（会被转换成小写字母）...
% inspired by http://www.toolsgeek.com/upsidedown.php
%
% exmaple:
%| ?- upside_down_text_gen("http://www.toolsgeek.com/upsidedown.php", R), atom_codes(A, R).
%A = 'dɥd˙uʍopǝpısdn/ɯoɔ˙ʞǝǝƃslooʇ˙ʍʍʍ//:dʇʇɥ',
%R = [100,613,100,729,117,653,111,112,477,112|...] ? ;
%no
upside_down_text_gen(Text, Result) :-
        upside_down_text_gen_aux(Text, [], Result).
upside_down_text_gen_aux([], R, R).
upside_down_text_gen_aux([H|T], Acc, R) :-
        to_lowercase(H, Hl),
        ( udt(Hl, Hlt) ->
          true
        ; Hlt = [H] ),
        append(Hlt, Acc, Acc1),
        upside_down_text_gen_aux(T, Acc1, R).
udt(0'0, "0").
udt(0'1, "⇂").
udt(0'2, "ᄅ").
udt(0'3, "ᄐ").
udt(0'4, "ㄣ").
udt(0'5, "ގ").
udt(0'6, "9").
udt(0'7, "ㄥ").
udt(0'8, "8").
udt(0'9, "6").
udt(0'!, "¡").
udt(0'\", ",,").
udt(0'\', ",").
udt(0'(, ")").
udt(0'), "(").
udt(0',, "'").
udt(0'., "˙").
udt(0';, "؛").
udt(0'<, ">").
udt(0'>, "<").
udt(0'?, "¿").
udt(0'[, "]").
udt(0'], "[").
udt(0'_, "‾").
udt(0'`, ",").
udt(0'a, "ɐ").
udt(0'b, "q").
udt(0'c, "ɔ").
udt(0'd, "p").
udt(0'e, "ǝ").
udt(0'f, "ɟ").
udt(0'g, "ƃ").
udt(0'h, "ɥ").
udt(0'i, "ı").
udt(0'j, "ɾ").
udt(0'k, "ʞ").
udt(0'l, "l").
udt(0'm, "ɯ").
udt(0'n, "u").
udt(0'o, "o").
udt(0'p, "d").
udt(0'q, "b").
udt(0'r, "ɹ").
udt(0's, "s").
udt(0't, "ʇ").
udt(0'u, "n").
udt(0'v, "ʌ").
udt(0'w, "ʍ").
udt(0'x, "x").
udt(0'y, "ʎ").
udt(0'z, "z").
udt(0'{, "}").
udt(0'}, "{").
udt(0'¡, "!").
udt(0'¿, "?").


