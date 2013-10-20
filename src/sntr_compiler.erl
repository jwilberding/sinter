%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) Erlware, LLC.
%%% @doc
-module(sntr_compiler).

-export([compile/2,
         format_error/1]).

-include("sinter.hrl").
-include_lib("yamerl/include/yamerl_errors.hrl").

-record(object, {name :: binary(),
                 fields=[] :: [{FieldName::atom(), sntr_type:t()}]}).

-record(field, {name :: binary(),
                required=false :: boolean(),
                default_value=<<"undefined">>,
                type :: sntr_type:t()}).

%%============================================================================
%% types
%%============================================================================

%%============================================================================
%% API
%%============================================================================
compile(_State, {ObjectName, Details}) ->
    Fields = proplists:get_value(<<"fields">>, Details),
    _Plugins = resolve_plugins(proplists:get_value(<<"plugins">>, Details, [])),
    #object{name=ObjectName,
            fields=resolve_fields(Fields)};
compile(State, FileToCompile)
  when erlang:is_list(FileToCompile) orelse
       erlang:is_binary(FileToCompile) ->
    try yamerl_yamler_compat:load_file(FileToCompile) of
        {ok, [DataObjects]} ->
            lists:foreach(fun(DataObject) -> compile(State, DataObject) end,
                          DataObjects);
        InvalidFormat ->
            ?SNTR_ERROR({invalid_format, InvalidFormat})
    catch
        throw:{yamerl_exception, Detail} ->
            ?SNTR_ERROR({yamerl_errors, FileToCompile, Detail})
    end.

format_error({error_group, Errors}) ->
    [format_error(Error) || Error <- Errors];
format_error({invalid_value, Yml}) ->
    io_lib:format("Invalid field value ~p", [Yml]);
format_error({invalid_format, Format}) ->
    io_lib:format("Invalid data object format: ~p", [Format]);
format_error({yamerl_errors, FileToCompile, Details}) ->
    [format_yamerl_error(FileToCompile, Detail) || Detail <- Details].

format_yamerl_error(FileToCompile,
                    #yamerl_parsing_error{text=Text,
                                          line=Line,
                                          column=Column}) ->
    io_lib:format("~s:~p:~p ~s~n", [FileToCompile, Line, Column, Text]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
resolve_plugins(_) ->
    [].

resolve_fields(Fields) ->
    [resolve_field(Field) || Field <- Fields].

resolve_field({BinFieldName, FieldTypes}) ->
    compile_type(proplists:get_value(<<"type">>, FieldTypes),
                 FieldTypes,
                 #field{name=BinFieldName});
resolve_field(InvalidValue) ->
    ?SNTR_ERROR({invalid_value, InvalidValue}).


compile_type(TypeName) when erlang:is_binary(TypeName) ->
    ok;
compile_type([{TypeName, TypeValues}, {<<"arg">>, Args}]) ->
    ok;
compile_type([{<<"arg">>, Args}, {TypeName, TypeValue}]) ->


%%%===================================================================
%%% Test Functions
%%%===================================================================
