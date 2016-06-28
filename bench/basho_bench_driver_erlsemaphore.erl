-module(basho_bench_driver_erlsemaphore).

-export([init/0,
         new/1,
         run/4]).

-record(state, {
            semaphore :: atom()
        }).

init() ->
    application:ensure_all_started(erlsemaphore).

new(Id) ->
    Semaphore = list_to_atom("test" ++ integer_to_list(Id)),
    case erlsemaphore:get(Semaphore) of
        {error, not_found} ->
            lager:debug("creating semaphore: ~p", [Semaphore]),
            {ok, Semaphore} = erlsemaphore:new(Semaphore);
        _ ->
            lager:debug("semaphore already created: ~p", [Semaphore]),
            ok
    end,
    {ok, #state{semaphore = Semaphore}}.

run(wait, _KeyGen, ValueGen,
    #state{semaphore = Semaphore} = State) ->
    Value = ValueGen(),
    case erlsemaphore:wait(Semaphore, Value, [{no_wait, true}]) of
        ok -> {ok, State};
        {error, not_found} -> {error, not_found, State};
        {error, would_block} -> {error, would_block, State}
    end;
run(signal, _KeyGen, ValueGen,
    #state{semaphore = Semaphore} = State) ->
    Value = ValueGen(),
    case erlsemaphore:signal(Semaphore, Value) of
        ok -> {ok, State};
        {error, Error} -> {error, Error, State}
    end.
