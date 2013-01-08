:- module(stringutils, [
                          string_ltrim/2,
                          string_rtrim/2,
                          string_trim/2
                       ]).

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
        
            