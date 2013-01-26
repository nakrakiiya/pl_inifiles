% -*- coding: utf-8 -*-
:- module(stringutils, [
                          string_ltrim/2,
                          string_rtrim/2,
                          string_trim/2,
                          utf8_byte_seq_to_codes/2,
                          utf8_byte_seq_to_atom/2
                       ]).

:- mode utf8_byte_seq_to_codes(+, -),
        utf8_byte_seq_to_atom(+, -),
        string_trim(+, -),
        string_ltrim(+, -),
        string_rtrim(+, -).

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

            