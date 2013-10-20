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
-module(sntr_t_binary).
-behavior(sntr_type).

%% f9_dmt_type callbacks
-export([init/2,
         validate/2,
         default_value/1,
         format_error/2]).

%%%============================================================================
%%% type
%%%============================================================================
-export_type([t/0]).

-type t() :: binary().

%%%============================================================================
%%% API
%%%============================================================================
-spec init(sntr_type:params(), sntr_type:yaml_info()) -> term().
init(_SubTypes, Args) ->
    DefaultValue = proplists:get_value(<<"default">>, Args, <<"<<>>">>),
    {?MODULE, DefaultValue}.

%% @hidden
validate(_, Value) ->
    erlang:is_binary(Value).

default_value({?MODULE, DefaultValue}) ->
    DefaultValue.

format_error(_, _) ->
    <<"">>.
