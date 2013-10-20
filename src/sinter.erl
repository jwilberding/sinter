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
%%% @copyright  Erlware, LLC.
%%% @doc
-module(sinter).

-include("sinter.hrl").

-export([main/1]).

%%============================================================================
%% types
%%============================================================================
-export_type([error/0]).

-type error() :: {error, {Module::module(), Reason::any()}}.

%%============================================================================
%% API
%%============================================================================
-spec main([string()]) -> ok | {errors, [error()]} | {ok, sntr_state:t()}.
main(Args) ->
    OptSpecList = opt_spec_list(),
    Result = case getopt:parse(OptSpecList, Args) of
                 {ok, {Options, Targets}} ->
                     startup_dependencies(),
                     do([{caller, command_line} | Options], Targets);
                 {error, Detail} ->
                     ?SNTR_ERROR({opt_parse, Detail})
             end,
    case Result of
        {error, _} ->
            report_error(rlx_state:caller(rlx_state:new([], undefined),
                                          command_line),
                         Result);
        _ ->
            Result
    end.

-spec do(proplists:proplist(), [file:name()]) -> {ok, sntr_state:t()} | {errors, [error()]}.
do(Options, Targets) ->
    State = sntr_state:new(Options, Targets),
    Errors0 = [sntr_compiler:compile(State, Target)
               || Target <- Targets],
    Errors1 = [Error || Error <- Errors0, Error =/= ok],
    case {Errors1, sntr_state:caller(State)} of
        {[], command_line} ->
            init:halt(0);
        {Errors, command_line} ->
            lists:foreach(fun(Error) ->  report_error(State, Error) end, Errors),
            init:halt(127);
        {[], _} ->
            {ok, State};
        {Errors, _} ->
            {errors, Errors}
    end.

%%============================================================================
%% Internal Functions
%%============================================================================
-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
    [{output,  $o, "output",  {string, "./generated"},
      "The output for the generated *.erl files"},
     {header_output, $h, "header-output", {string, "./include"},
      "The output for the generated header files"}].

startup_dependencies() ->
    application:start(yamerl),
    application:start(sinter).

-spec report_error(rlx_state:t(), error()) -> none() | error().
report_error(State, Error) ->
    case Error of
        {error, {sinter, {opt_parse, _}}} ->
            io:format(standard_error, format_error(Error), []),
            usage();
        _ ->
            io:format(standard_error, format_error(Error), [])
    end,
    case sntr_state:caller(State) of
        command_line ->
            erlang:halt(127);
        api ->
            Error
    end.

-spec format_error(Reason::term()) -> string().
format_error({opt_parse, {invalid_option, Opt}}) ->
    io_lib:format("invalid option ~s~n", [Opt]);
format_error({opt_parse, Arg}) ->
    io_lib:format("~p~n", [Arg]);
format_error({error, {sinter, Reason}}) ->
    format_error(Reason);
format_error({error, {Module, Reason}}) ->
    io_lib:format("~s~n", [Module:format_error(Reason)]).

-spec usage() -> ok.
usage() ->
    getopt:usage(opt_spec_list(), "sinter", "[*sinter description files*]").
