% -*- coding: utf-8 -*-
:- module(inifiles2, [
                        read_inifile/2,
                        read_inifile/3,
                        ini_get_property/3,
                        ini_ensure_property/4,
                        ini_get_section/2,
                        ini_ensure_section/3,
                        ini_get_keyvalue/4,
                        ini_ensure_keyvalue/5,
                        save_to_inifile/1,
                        save_to_inifile/2,
                        save_to_inifile/3
                     ]).
:- use_module(library(types)).
:- use_module(library(lists)).
:- use_module(stringutils).

:- mode
           read_inifile(+, -),
           ini_get_property(+, ?, ?), 
           ensure_key_value(+, +, +, -),
           ini_ensure_property(+, +, +, -),
           ini_get_section(+, ?),
           ini_ensure_section(+, +, -),
           ini_get_keyvalue(+, ?, ?, ?),
           ini_ensure_keyvalue(+, +, +, +, -),
           save_to_inifile(+),
           save_to_inifile(+, +).

% FileSpec = '$inifile'(Properties, SectionList)
% Properties = [[Name|Value], ...]
% SectionList = [[Name|KeyValueList], ...]
% KeyValueList = [[Key|Value], ...]

check_inispec('$inifile'([[PropName1|PropValue1]|PropRest], SL)) :-
        check_kvl([[PropName1|PropValue1]|PropRest]),
        check_section_list(SL),
        !.
check_inispec(IniSpec) :-
        raise_exception(invalid_inispec(IniSpec)).
check_section_list([]) :- !.
check_section_list([[Name|KVL] | T]) :- 
        atom(Name),
        check_kvl(KVL),
        check_section_list(T).
check_kvl([]) :- !.
check_kvl([[Name|Value]|T]) :- 
        atom(Name),
        ( atom(Value) ; number(Value) ),
        check_kvl(T).
        
        

% read_inifile(+FileSpec, -IniSpec)  TODO perform more test 
read_inifile(FileSpec, IniSpec) :-
        read_inifile(FileSpec, 'UTF-8', IniSpec).
% read_inifile(+FileSpec, +Encoding, -IniSpec)  TODO perform more test
read_inifile(FileSpec, Encoding, '$inifile'([[path|FileSpec]], SectionList)) :-
        open(FileSpec, read, Stream, [encoding(Encoding)]),
        call_cleanup(read_inifile_aux(SectionList, Stream), close(Stream)).
% read_inifile_aux(+Stream, -SectionList)
read_inifile_aux(SectionList, Stream) :-
        read_inifile_aux([], SectionList, Stream, _).
read_inifile_aux(SL, SLResult, Stream, CurrentSection) :-
        read_line(Stream, Line),
        ( Line = end_of_file 
        ->
          % merge the result
          ( var(CurrentSection) % no value
          ->
            SL1 = SL
          ; 
            CurrentSection = [SectionName|KeyValueList],
            reverse(KeyValueList, KVLNew),
            SL1 = [ [SectionName | KVLNew] |SL] ),
          reverse(SL1, SLResult)
        ; % parse the line
          read_inifile_process_line(SL, SLNew, Line, CurrentSection, SNew),
          read_inifile_aux(SLNew, SLResult, Stream, SNew) ).
%read_inifile_process_line(+SectionList, -SLNew, +Line, +CurrentSection, -SectionNew),
read_inifile_process_line(SL, SL, Line, S, S) :-
        (  comment_line(Line, [])
        ;  empty_line(Line, []) ),
        !. % skip
read_inifile_process_line(SL, SLNew, Line, CurrentSection, SNew) :-
        section_header_line(NewSection, Line, []), 
        !,
        % add to section list
        SNew = [ NewSection | [] ],
        ( var(CurrentSection) 
        ->
          SLNew = SL
        ;
          CurrentSection = [SectionName|KeyValueList],
          reverse(KeyValueList, KVLNew),
          SLNew = [ [SectionName|KVLNew] | SL] ).
read_inifile_process_line(SL, SL, Line, [SectionName|KeyValueList], [SectionName|KeyValueListNew]) :-
        key_value_pair_line(Key, Value, Line, []),
        !,
        KeyValueListNew = [ [Key|Value] | KeyValueList ].
read_inifile_process_line(_,_,_,_,_) :-
        raise_exception(invalid_file_format('The content of the file is invalid.')).

%% getters and setters for the sections 
%% TODO test the setter and getters

%% Add or set a Value associated with Name in KVList and returns it as NewKVList
%% it might replace all Values associated with all the same Names
% ensure_key_value(+KVList, +Name, +Value, -NewKVList)
ensure_key_value(KVList, Name, Value, NewKVList) :-
        ensure_key_value(KVList, Name, Value, not_found, NewKVList).
ensure_key_value([], Name, Value, not_found, [[Name|Value]]) :- !.
ensure_key_value([], _, _, _, []) :- !. % found
ensure_key_value([[Name|_]|T], Name, Value, _, [[Name|Value]|T1]) :- !,
        ensure_key_value(T, Name, Value, found, T1).
ensure_key_value([H|T], Name, Value, Found, [H|T1]) :-
        ensure_key_value(T, Name, Value, Found, T1).
        

ensure_key(KVList, Name, NewKVList) :-
        ensure_key(KVList, Name, not_found, NewKVList).
ensure_key([], Name, not_found, [[Name|[]]]) :- !.
ensure_key([], _, _, []) :- !. % found
ensure_key([[Name|Value]|T], Name, _, [[Name|Value]|T1]) :- !,
        ensure_key(T, Name, found, T1).
ensure_key([H|T], Name, Found, [H|T1]) :-
        ensure_key(T, Name, Found, T1).

        
%% getters and setters for properties
% ini_get_property(+IniSpec, ?Name, ?Value)
ini_get_property('$inifile'(PropList, _), Name, Value) :-
        member([Name | Value], PropList).

% ini_ensure_property(+Name, +Value, +IniSpec, -NewIniSpec)
ini_ensure_property(Name, Value, '$inifile'(PropList, KeyValueList), '$inifile'(NewPropList, KeyValueList)) :-
        % check arguments
        must_be(Name, atom, ini_ensure_property(Name, Value, '$inifile'(PropList, KeyValueList), '$inifile'(NewPropList, KeyValueList)), 1),
        must_be(Value, atom, ini_ensure_property(Name, Value, '$inifile'(PropList, KeyValueList), '$inifile'(NewPropList, KeyValueList)), 2),
        % do it
        ensure_key_value(PropList, Name, Value, NewPropList).
        
        
% ini_get_section(+IniSpec, ?SectionName)
ini_get_section('$inifile'(_, SectionList), SectionName) :-
        member([SectionName | _], SectionList).

% ini_ensure_section(+SectionName, +IniSpec, -NewIniSpec)
% ensure the section named SectionName exists
ini_ensure_section(SectionName, '$inifile'(PropList, SectionList), '$inifile'(PropList, NewSectionList)) :-
        % check arguments
        must_be(SectionName, atom, ini_ensure_section(SectionName, '$inifile'(PropList, SectionList), '$inifile'(PropList, NewSectionList)), 2),
        % do it
        ensure_key(SectionList, SectionName, NewSectionList).


% ini_get_keyvalue(+IniSpec, ?SectionName, ?Key, ?Value) :-
ini_get_keyvalue('$inifile'(_, SectionList), SectionName, Key, Value) :-
        member([SectionName | KeyValuePairs], SectionList),
        member([Key|Value], KeyValuePairs).


% ini_ensure_keyvalue(+SectionName, +Key, +Value, +IniSpec, -NewIniSpec)        
ini_ensure_keyvalue(SectionName, Key, Value, IniSpec, '$inifile'(PropList, NewSL1)) :-
        % check arguments
        must_be(SectionName, ground, ini_ensure_keyvalue(SectionName, Key, Value, IniSpec, '$inifile'(PropList, NewSL1)), 1),
        must_be(Key, ground, ini_ensure_keyvalue(SectionName, Key, Value, IniSpec, '$inifile'(PropList, NewSL1)), 2),
        must_be(Value, ground, ini_ensure_keyvalue(SectionName, Key, Value, IniSpec, '$inifile'(PropList, NewSL1)), 3),
        % do it
        ini_ensure_section(SectionName, IniSpec, '$inifile'(PropList, SL1)),
        ini_ensure_keyvalue_aux(SL1, NewSL1, SectionName, Key, Value).
% ini_ensure_keyvalue_aux(+SectionList, -NewSectionList, +SectionName, +Key, +Value) :-        
ini_ensure_keyvalue_aux([], [], _, _, _) :- !.
ini_ensure_keyvalue_aux([[SectionName | KVL] | ST], [[SectionName | KVL1] | ST1], SectionName, Key, Value) :- !,
        ensure_key_value(KVL, Key, Value, KVL1),
        ini_ensure_keyvalue_aux(ST, ST1, SectionName, Key, Value).
ini_ensure_keyvalue_aux([H | ST], [H | ST1], SectionName, Key, Value) :- 
        ini_ensure_keyvalue_aux(ST, ST1, SectionName, Key, Value).
                                                                       

% save_to_inifile(+IniSpec)
save_to_inifile(IniSpec) :-
        ini_get_property(IniSpec, path, Path),
        save_to_inifile(IniSpec, Path, 'UTF-8').
% save_to_inifile(+IniSpec, +NewPath)
save_to_inifile(IniSpec, NewPath) :-
        save_to_inifile(IniSpec, NewPath, 'UTF-8').
% save_to_inifile(+IniSpec, +NewPath, +Encoding)
save_to_inifile(IniSpec, NewPath, Encoding) :-
        check_inispec(IniSpec),
        open(NewPath, write, Stream, [encoding(Encoding)]),
        save_to_inifile_save_sections(Stream, IniSpec),
        close(Stream).
save_to_inifile_save_sections(Stream, IniSpec) :-
        ini_get_section(IniSpec, SectionName),
        format(Stream, '[~w]~N', SectionName),
        save_to_inifile_save_keyvalues(Stream, IniSpec, SectionName),
        nl(Stream),
        fail. % loop
save_to_inifile_save_sections(_,_).
save_to_inifile_save_keyvalues(Stream, IniSpec, SectionName) :-
        ini_get_keyvalue(IniSpec, SectionName, Key, Value),
        format(Stream, '~w = ~w~N', [Key, Value]),
        fail.
save_to_inifile_save_keyvalues(_,_,_).        












%%%%%%%%%%%%%%%%%%%%% syntax of ini files %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 空字符串
empty --> [].

% 任意长度任意字符
anything(GotString) -->
        anything_but(GotString, []).
anything -->
        anything_but(_, []).

% 任何不包含But列出来的字符的字符串
% anything_but(-GotString, +But)
anything_but([X|Rest], But) -->
        [X],
        { \+ memberchk(X, But) },
        anything_but(Rest, But).
anything_but([], _) -->
        empty.

% 任何不包含But列出来的字符的非空字符串
nonempty_but([X|Rest], But) -->
        [X],
        { \+ memberchk(X, But) },
        anything_but(Rest, But).      
nonempty_but([], But) -->
        [X],
        { memberchk(X, But) }.
        

% 任意长度的空白
whitespace -->
        whitespace_char, 
        whitespace.
whitespace -->
        empty.
whitespace_char -->
        [32]; [9].

% Empty Line
empty_line -->
        whitespace.

%Comments:
%        # ...
%        // ...
%        ; ...
comment_line -->
        whitespace, comment_marker, anything.
comment_marker -->
        "#" ; "//" ; ";".

% Section Header Line
% section_header_line(-SectionName)
section_header_line(SectionName) -->
        whitespace, "[", nonempty_but(SectionName1, "]"), "]", whitespace,
        { 
           string_trim(SectionName1, SectionNameCodes),
           atom_codes(SectionName, SectionNameCodes)
        }.

% Key = Value pair
key_value_pair_line(Key, Value) -->
        nonempty_but(Key1, "="), "=", anything(Value1),
        { 
           string_trim(Key1, KeyCodes),
           string_trim(Value1, ValueCodes),
           atom_codes(Key, KeyCodes),
           catch( number_codes(Value, ValueCodes), % try convert to number 
                  _, 
                  atom_codes(Value, ValueCodes))
        }.

%%%%%%%%%%%%%%%%%%%%% end of syntax of ini files %%%%%%%%%%%%%%%%%%%%%%%%


end_of_file.

%% TEST CODE

read_inifile('C:\\trac\\projects\\my-project\\conf\\trac.ini', X).