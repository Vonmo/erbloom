-module(bloom_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        {group, bloom},
        {group, perf}
    ].

groups() ->
    [
        {bloom,
            [parallel, shuffle],
                [new, serialize, deserialize, set, check, clear]},

        {perf,
            [shuffle],
                [perf_sequential_csc, perf_parallel_read, perf_sequential_csc_large]}
    ].


%% =============================================================================
%% init
%% =============================================================================
init_per_group(_Group, Config) ->
    ok = application:load(erbloom),
    {ok, _} = application:ensure_all_started(erbloom, temporary),
    [{init, true} | Config].


%% =============================================================================
%% end
%% =============================================================================
end_per_group(_Group, _Config) ->
    ok = application:stop(erbloom),
    ok = application:unload(erbloom),
    ok.


%% =============================================================================
%% group: bloom
%% =============================================================================
new(_) ->
    {ok, Ref} = bloom:new(10,80),
    true = is_reference(Ref),
    ok.

serialize(_)->
    {ok, Ref} = bloom:new(10,80),
    {ok,{[0,0,0,0,0,0,0,0,0,0],
         80,1,
         {_,_},
         {_,_}}} = bloom:serialize(Ref),
    ok.

deserialize(_)->
    {ok, Filter1} = bloom:new(10,80),
    {ok, _} = bloom:serialize(Filter1),
    Key = "test_key",
    {ok, false} = bloom:check(Filter1, Key),
    {ok, Filter11} = bloom:set(Filter1, Key),
    {ok, true} = bloom:check(Filter11, Key),
    {ok, Filter12} = bloom:clear(Filter11),
    {ok, false} = bloom:check(Filter12, Key),

    {ok,{Bitmap,NumBits,NumFuns,{Sv00,Sv01},{Sv10,Sv11}}} = bloom:serialize(Filter11),
    {ok, Filter13} = bloom:deserialize(Bitmap,NumBits,NumFuns,Sv00,Sv01,Sv10,Sv11),
    {ok, true} = bloom:check(Filter13, Key),
    ok.

set(_) ->
    {ok, Ref} = bloom:new(10,80),
    Key = "binkeyfortest",
    {ok, Ref} = bloom:set(Ref, Key),
    ok.

check(_) ->
    Key = "binkeyfortest",
    {ok, Ref} = bloom:new(10,80),
    {ok, false} = bloom:check(Ref, Key),
    {ok, Ref1} = bloom:set(Ref, Key),
    {ok, true} = bloom:check(Ref1, Key),
    ok.

clear(_) ->
    Key = "binkeyfortest",
    {ok, Ref} = bloom:new(10,80),
    {ok, Ref1} = bloom:set(Ref, Key),
    {ok, true} = bloom:check(Ref1, Key),
    {ok, Ref2} = bloom:clear(Ref1),
    {ok, false} = bloom:check(Ref2, Key),
    ok.


%% =============================================================================
%% group: perf
%% =============================================================================
perf_sequential_csc(_)->
    R = perftest:sequential(100, fun()->
        Key = uuid:uuid_to_string(uuid:get_v4()),
        {ok, Ref} = bloom:new(10,80),
        {ok, Ref} = bloom:set(Ref, Key),
        {ok, true} = bloom:check(Ref, Key)
    end),
    true = R > 3000,
    ok.

perf_sequential_csc_large(_)->
    R = perftest:sequential(100, fun()->
        Key = uuid:uuid_to_string(uuid:get_v4()),
        {ok, Ref} = bloom:new(9585059,1000000),
        {ok, Ref} = bloom:set(Ref, Key),
        {ok, true} = bloom:check(Ref, Key),
        ok
    end),
    true = R > 3000,
    ok.

perf_parallel_read(_)->
    Key = uuid:uuid_to_string(uuid:get_v4()),
    {ok, Ref} = bloom:new(10,80),
    {ok, Ref} = bloom:set(Ref, Key),
    R = perftest:comprehensive(1000, fun()->
        {ok, true} = bloom:check(Ref, Key)
    end),
    true = lists:all(fun(E)-> E >=3500 end, R),
    ok.
