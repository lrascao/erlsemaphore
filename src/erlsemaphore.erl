%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Luis Rascão.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(erlsemaphore).

-on_load(init/0).

-export([new/1,
         new/2,
         get/1,
         delete/1,
         wait/2,
         wait/3,
         signal/2]).

-type new_opts() :: proplists:proplist().
-export_type([new_opts/0]).
-type signal_opts() :: proplists:proplist().
-export_type([signal_opts/0]).

-spec new(Name :: atom()) ->
        {error, already_exists | semaphore_creation_failed} | {ok, atom()}.
new(Name) ->
    new(Name, [{n, 1}]).

-spec get(Name :: atom()) ->
        {error, not_found} | {ok, atom()}.
get(Name) ->
    nif_get(Name).

-spec new(Name :: atom(),
          Opts :: new_opts()) ->
        {error, already_exists | semaphore_creation_failed} | {ok, atom()}.
new(Name, Opts) ->
    N = proplists:get_value(n, Opts),
    nif_new(Name, N).

-spec delete(Name :: atom()) -> error | {ok, atom()}.
delete(Name) ->
    nif_delete(Name).

-spec wait(Name :: atom(),
           N :: non_neg_integer()) -> {error, not_found | would_block} | ok.
wait(Name, N) ->
    wait(Name, N, [{no_wait, true}]).

-spec wait(Name :: atom(),
           N :: non_neg_integer(),
           Opts :: signal_opts()) -> {error, not_found | would_block} | ok.
wait(Name, N, Opts) ->
    NoWait = case proplists:get_value(no_wait, Opts, true) of
                true -> 1;
                false -> 0
             end,
    nif_wait(Name, N, NoWait).

-spec signal(Name :: atom(),
             N :: non_neg_integer()) -> {error, not_found} | ok.
signal(Name, N) ->
    nif_signal(Name, N).

nif_new(_, _) ->
    erlang:nif_error({error, not_loaded}).

nif_get(_) ->
    erlang:nif_error({error, not_loaded}).

nif_delete(_) ->
    erlang:nif_error({error, not_loaded}).

nif_wait(_, _, _) ->
    erlang:nif_error({error, not_loaded}).

nif_signal(_, _) ->
    erlang:nif_error({error, not_loaded}).

init() ->
    SoName =
        case code:priv_dir(erlsemaphore) of
            {error, bad_name} ->
                case code:which(?MODULE) of
                    Filename when is_list(Filename) ->
                        filename:join([filename:dirname(Filename), "../priv", "erlsemaphore"]);
                    _ ->
                        filename:join("../priv", "erlsemaphore")
                end;
            Dir ->
                filename:join(Dir, "erlsemaphore")
        end,
    erlang:load_nif(SoName, 0).
