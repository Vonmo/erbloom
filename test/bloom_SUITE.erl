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
                [new, serialize, deserialize, set, check, check_and_set, clear, to_from_bin]},

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
    {ok,{<<0,0,0,0,0,0,0,0,0,0>>,
         80,1,
         {_,_},
         {_,_}}} = bloom:serialize(Ref),
    ok.

deserialize(_)->
    {ok, Filter1} = bloom:new(10,80),
    {ok, _} = bloom:serialize(Filter1),
    Key = "test_key",
    false = bloom:check(Filter1, Key),
    ok = bloom:set(Filter1, Key),
    true = bloom:check(Filter1, Key),
    {ok, Serialized={Bitmap,NumBits,NumFuns,{Sv00,Sv01},{Sv10,Sv11}}} = bloom:serialize(Filter1),
    {ok, Filter13} = bloom:deserialize(Bitmap,NumBits,NumFuns,Sv00,Sv01,Sv10,Sv11),
    {ok, Filter14} = bloom:deserialize(Serialized),
    ok = bloom:clear(Filter1),
    false = bloom:check(Filter1, Key),
    true = bloom:check(Filter13, Key),
    true = bloom:check(Filter14, Key),
    ok.

set(_) ->
    {ok, Ref} = bloom:new(10,80),
    Key = "binkeyfortest",
    ok = bloom:set(Ref, Key),
    ok.

check(_) ->
    Key = "binkeyfortest",
    {ok, Ref} = bloom:new(10,80),
    false = bloom:check(Ref, Key),
    ok = bloom:set(Ref, Key),
    true = bloom:check(Ref, Key),
    %% we can check a serialized bloom
    {ok, Ser} = bloom:serialize(Ref),
    true = bloom:check(Ser, Key),
    %% we can check a to_bin bloom
    Bin = bloom:to_bin(Ref),
    true = bloom:check(Bin, Key),
    ok.

check_and_set(_) ->
    Key = "binkeyfortest",
    {ok, Ref} = bloom:new(10,80),
    false = bloom:check_and_set(Ref, Key),
    true = bloom:check(Ref, Key),
    true = bloom:check_and_set(Ref, Key),
    bloom:clear(Ref),
    false = bloom:check(Ref, Key),
    false = bloom:check_and_set(Ref, Key),
    ok.

clear(_) ->
    Key = "binkeyfortest",
    {ok, Ref} = bloom:new(10,80),
    ok = bloom:set(Ref, Key),
    true = bloom:check(Ref, Key),
    ok = bloom:clear(Ref),
    false = bloom:check(Ref, Key),
    ok.

to_from_bin(_) ->
    Key = <<"binkeyfortest">>,
    Key2 = <<"binkeyfortestingmore">>,
    {ok, Ref} = bloom:new(10,80),
    false = bloom:check(Ref, Key),
    ok = bloom:set(Ref, Key),
    true = bloom:check(Ref, Key),
    Bin = bloom:to_bin(Ref),
    true = bloom:check(Bin, Key),
    false = bloom:check(Bin, Key2),
    {ok, Ref2} = bloom:from_bin(Bin),
    true = bloom:check(Ref2, Key),
    false = bloom:check(Ref2, Key2),
    bloom:set(Ref2, Key2),
    true = bloom:check(Ref2, Key2),
    false = bloom:check(Bin, Key2),
    ok.

%% =============================================================================
%% group: perf
%% =============================================================================
perf_sequential_csc(_)->
    R = perftest:sequential(100, fun()->
        Key = uuid:uuid_to_string(uuid:get_v4()),
        {ok, Ref} = bloom:new(10,80),
        ok = bloom:set(Ref, Key),
        true = bloom:check(Ref, Key)
    end),
    true = R > 3000,
    ok.

perf_sequential_csc_large(_)->
    R = perftest:sequential(100, fun()->
        Key = uuid:uuid_to_string(uuid:get_v4()),
        {ok, Ref} = bloom:new(9585059,1000000),
        ok = bloom:set(Ref, Key),
        true = bloom:check(Ref, Key),
        ok
    end),
    true = R > 3000,
    ok.

perf_parallel_read(_)->
    Key = uuid:uuid_to_string(uuid:get_v4()),
    {ok, Ref} = bloom:new(10,80),
    ok = bloom:set(Ref, Key),
    R = perftest:comprehensive(1000, fun()->
        true = bloom:check(Ref, Key)
    end),
    true = lists:all(fun(E)-> E >=3500 end, R),
    ok.
