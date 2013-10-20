%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
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
%%% @copyright (C) 2012 Erlware, LLC.
-include_lib("erlware_commons/include/ec_cmd_log.hrl").

-define(SNTR_ERROR, ?EC_ERROR).
-define(SNTR_WARN, ?EC_WARN).
-define(SNTR_INFO, ?EC_INFO).
-define(SNTR_DEBUG, ?EC_DEBUG).

%% This is the default form of error messages for the Relx
%% system. It is expected that everything that returns an error use
%% this and that they all expose a format_error/1 message that returns
%% an iolist.
-define(SNTR_ERROR(Reason),
        {error, {?MODULE, Reason}}).
