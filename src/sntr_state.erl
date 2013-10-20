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
-module(sntr_state).

-export([new/2,
         output_dir/1,
         header_output_dir/1,
         caller/1,
         caller/2,
         templates/1,
         templates/2
         template/2]).

-record(t, {header_output_dir :: file:name(),
            output_dir :: file:name(),
            sntr_files :: [file:name()],
            caller=api :: caller()}).

%%============================================================================
%% Types
%%============================================================================

-export_type([t/0,
             caller/0]).

-opaque t() :: record(t).
-type caller() :: api | command_line.

%%============================================================================
%% API
%%============================================================================
-spec new(proplists:proplist(), [file:name()]) -> t().
new(PropList, Files) ->
    #t{header_output_dir = proplists:get_value(header_output, PropList),
       output_dir = proplists:get_value(output, PropList),
       caller = proplists:get_value(caller, PropList, api),
       sntr_files = Files}.

-spec output_dir(t()) -> file:name().
output_dir(#t{output_dir=Dir}) ->
    Dir.

-spec header_output_dir(t()) -> file:name().
header_output_dir(#t{header_output_dir=Dir}) ->
    Dir.

-spec caller(t()) -> caller().
caller(#t{caller=Caller}) ->
    Caller.

-spec caller(t(), caller()) -> t().
caller(T, NewCaller) ->
    T#t{caller=NewCaller}.
