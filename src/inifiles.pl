% -*- coding: utf-8 -*-
:- module(inifiles, [
                       read_inifile/2,
                       ini_get_property/3,
                       ini_get_section/2,
                       ini_ensure_section/2,
                       ini_get_keyvalue/4,
                       ini_ensure_keyvalue/4,
                       save_to_inifile/1,
                       save_to_inifile/2
                    ]).
:- use_module(library(types)).
:- use_module(stringutils).

:- mode read_inifile(+, -),
        ini_get_property(+, +, -),
        ini_get_section(+, -),
        ini_ensure_section(+, +),
        ini_get_keyvalue(+, ?, ?, ?),
        ini_ensure_keyvalue(+, +, +, +),
        save_to_inifile(+),
        save_to_inifile(+, +).

% TODO do not use any global state

:- dynamic 
           '$id'/1,
           '$inifile'/1,
           '$inifile'/2,
           '$inifile_props'/2,
           '$inifile'/4.
%        '$inifile_props'(ID, [path(Path)])
%        '$inifile'(ID).
%        '$inifile'(ID, SectionName).
%        '$inifile'(ID, SectionName, Key, Value).
%        SectionName, Key, Value are atoms

:- volatile % do not save the data into the saved-state file
           '$id'/1,
           '$inifile'/1,
           '$inifile'/2,
           '$inifile_props'/2,
           '$inifile'/4.

newid(ID) :-
        once('$id'(Current)),
        !,
        ID is Current + 1,
        asserta('$id'(ID)),
        retractall('$id'(Current)).
newid(0). % the initial value

% new_inispec(-IniSpec, +Path)
new_inispec('$inifile'(NewCurrent), Path) :-
        nonvar(Path),
        newid(NewCurrent),
        asserta('$inifile'(NewCurrent)), 
        asserta('$inifile_props'(NewCurrent, [path(Path)])).

% valid_inispec(?IniSpec)
valid_inispec('$inifile'(X)) :-
        nonvar(X),
        '$inifile'(X),
        !.
valid_inispec(X) :-
        raise_exception(type_error(inispec,X)). 

delete_inispec('$inifile'(X)) :-
        retractall('$inifile'(X)),
        retractall('$inifile'(X,_)),
        retractall('$inifile_props'(X,_)),
        retractall('$inifile'(X,_,_,_)).

% ini_path(+IniSpec, ?Path)
% unifies the path to Path associated with IniSpec
ini_path('$inifile'(ID), Path) :-
        valid_inispec('$inifile'(ID)),
        '$inifile_props'(ID, Properties),
        member(path(Path), Properties).

%% getters and setters for properties
ini_get_property(IniSpec, path, Path) :-
        ini_path(IniSpec, Path).

%% getters and setters for the sections

% ini_get_section(+IniSpec, -SectionName)
ini_get_section('$inifile'(ID), SectionName) :-
        '$inifile'(ID, SectionName).

% ini_get_section(+IniSpec, +SectionName)
% ensure the section named SectionName exists
ini_ensure_section('$inifile'(ID), SectionName) :-
        nonvar(SectionName),
        valid_inispec('$inifile'(ID)),
        ( '$inifile'(ID, SectionName) ->
          true
        ;
          assertz('$inifile'(ID, SectionName))).

% ini_get_keyvalue(+IniSpec, ?SectionName, ?Key, ?Value) :-
ini_get_keyvalue('$inifile'(ID), SectionName, Key, Value) :-
        '$inifile'(ID, SectionName, Key, Value).

% ini_ensure_keyvalue(+IniSpec, +SectionName, +Key, +Value)        
ini_ensure_keyvalue('$inifile'(ID), SectionName, Key, Value) :-
        % check the arguments
        must_be('$inifile'(ID), ground, ini_ensure_keyvalue('$inifile'(ID), SectionName, Key, Value), 1),
        must_be(SectionName, ground, ini_ensure_keyvalue('$inifile'(ID), SectionName, Key, Value), 2),
        must_be(Key, ground, ini_ensure_keyvalue('$inifile'(ID), SectionName, Key, Value), 3),
        must_be(Value, ground, ini_ensure_keyvalue('$inifile'(ID), SectionName, Key, Value), 4),
        %%
        ( '$inifile'(ID, SectionName, Key, Value) ->
          true
        ;
          retractall('$inifile'(ID, SectionName, Key, _)),
          assert('$inifile'(ID, SectionName, Key, Value))).

% read_inifile(+FileSpec, -IniSpec)  TODO perform more test 
read_inifile(FileSpec, IniSpec) :-
        open(FileSpec, read, Stream),
        new_inispec(IniSpec, FileSpec),
        catch( call_cleanup(read_inifile_aux(IniSpec, Stream, _), close(Stream)), 
               E, 
               (delete_inispec(IniSpec), raise_exception(E)) ).
read_inifile_aux(IniSpec, Stream, CurrentSection) :-
        read_line(Stream, Line),
        ( Line = end_of_file 
        ->
          true
        ; read_inifile_process_line(IniSpec, CurrentSection, Line, NewSection),
          read_inifile_aux(IniSpec, Stream, NewSection)).
read_inifile_process_line(_, CurrentSection, Line, CurrentSection) :-
        (  comment_line(Line, [])
        ;  empty_line(Line, []) ),
        !. % skip
read_inifile_process_line(IniSpec, _, Line, NewSection) :-
        section_header_line(NewSection, Line, []), 
        !,
        assertz('$inifile'(IniSpec, NewSection)).
read_inifile_process_line(IniSpec, CurrentSection, Line, CurrentSection) :-
        nonvar(CurrentSection), % ensure in a section
        key_value_pair_line(Key, Value, Line, []),
        !,
        assertz('$inifile'(IniSpec, CurrentSection, Key, Value)).
read_inifile_process_line(_,_,_,_) :-
        raise_exception(invalid_file_format('The content of the file is invalid.')).

% save_to_inifile(+IniSpec)
save_to_inifile(IniSpec) :-
        ini_get_property(IniSpec, path, Path),
        save_to_inifile(IniSpec, Path).
% save_to_inifile(+IniSpec, +NewPath)
save_to_inifile(IniSpec, NewPath) :-
        valid_inispec(IniSpec),
        open(NewPath, write, Stream),
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
        format(Stream, '~w=~w~N', [Key, Value]),
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
           atom_codes(Value, ValueCodes)
        }.

%%%%%%%%%%%%%%%%%%%%% end of syntax of ini files %%%%%%%%%%%%%%%%%%%%%%%%
