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
-module(sntr_type).

%% API
-export([new/1, new/2, new/3,
         validate/2,
         parse/1,
         format_error/2]).

-record(t, {name :: module(),
            params :: term()}).

%%%============================================================================
%%% Behavior callbacks
%%%============================================================================

-callback init([t()], yaml_info()) -> term().
-callback validate(params(), any()) -> ok | error().
-callback format_error(params(), error()) -> iolist().
-callback default_value(params()) -> term().

%%%============================================================================
%%% Types
%%%============================================================================

-export_type([t/0,
              params/0,
              yaml_info/0,
              error/0]).

-opaque t() :: record(t).
-type yaml_info() :: [any()].
-type params() :: [t()].
-type error() :: {error, Reason::any()}.

%%%============================================================================
%%% API
%%%============================================================================
-spec new(TypeName::atom()) -> t().
new(TypeName) ->
    new(TypeName, [], []).

-spec new(TypeName::atom(), [t()]) -> t().
new(TypeName, Types) ->
    new(TypeName, Types, []).

-spec new(TypeName::atom(), [t()], yaml_info()) -> t() | error().
new(TypeName, Types, Yaml)
  when erlang:is_atom(TypeName),  erlang:is_list(Yaml) ->
    case is_type(TypeName) of
        true ->
            #t{name = TypeName, params = TypeName:init(Types, Yaml)};
        false ->
            new(to_sinter_type(TypeName), Types, Yaml)
    end.

-spec validate(t(), Value::any()) -> boolean().
validate(#t{name = TypeName, params = Params}, Value) ->
    TypeName:validate(Value, Params).

-spec format_error(t(), error()) -> boolean().
format_error(#t{name = TypeName, params = Params}, Error) ->
    TypeName:format_error(Error, Params).

-spec parse(yaml_info()) ->  {ok, {module(), yaml_info()}}.
parse(_Type) ->
    {ok, {foo, []}}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
to_sinter_type(TypeName) ->
    erlang:list_to_atom(
      erlang:binary_to_list(
        erlang:iolist_to_binary(io_lib:format("sntr_t_~s", [TypeName])))).

-spec is_type(module()) -> boolean().
is_type(ModuleName) ->
    try ModuleName:module_info(exports) of
        Exports ->
            lists:all(fun(Requirement) ->
                              lists:member(Requirement, Exports)
                      end, [{new, 1},
                            {validate, 2},
                            {format_error, 2}])
    catch
        error:undef ->
            false
    end.
