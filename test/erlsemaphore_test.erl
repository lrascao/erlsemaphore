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
-module(erlsemaphore_test).

-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [{<<"New works">>,
        fun() ->
            ?assertEqual({ok, test}, erlsemaphore:new(test))
        end},
       {<<"Detection of double creation works">>,
        fun() ->
            ?assertEqual({ok, test}, erlsemaphore:new(test)),
            ?assertEqual({error, already_exists}, erlsemaphore:new(test))
        end},
       {<<"Detection of double creation works">>,
        fun() ->
            ?assertEqual({ok, test}, erlsemaphore:new(test)),
            ?assertEqual({error, already_exists}, erlsemaphore:new(test))
        end},
       {<<"Signal/Wait works">>,
        fun() ->
            ?assertEqual({ok, test}, erlsemaphore:new(test)),
            ?assertEqual({error, would_block}, erlsemaphore:wait(test, 1, [{no_wait, true}])),
            ?assertEqual(ok, erlsemaphore:signal(test, 1)),
            ?assertEqual(ok, erlsemaphore:wait(test, 1, [{no_wait, true}]))
        end}
       ]
    }.

setup() -> ok.

teardown(_) ->
  ?assertEqual(ok, erlsemaphore:delete(test)),
  ok.
