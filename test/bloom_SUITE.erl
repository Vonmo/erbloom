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
      [new, serialize, deserialize, set, check, clear, check_and_set, check_serialized]},
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
  {ok, Ref} = bloom:new(10, 80),
  true = is_reference(Ref),
  0 = bloom:type(Ref),
  ok.

serialize(_) ->
  {ok, Ref} = bloom:new(10, 80),
  {ok, SerializedFilter} = bloom:serialize(Ref),
  true = is_binary(SerializedFilter),
  ok.

deserialize(_) ->
  {ok, Filter1} = bloom:new(10, 80),
  {ok, _} = bloom:serialize(Filter1),
  Key = "test_key",
  false = bloom:check(Filter1, Key),
  ok = bloom:set(Filter1, Key),
  true = bloom:check(Filter1, Key),
  {ok, Serialized} = bloom:serialize(Filter1),
  true = is_binary(Serialized),
  {ok, Filter13} = bloom:deserialize(Serialized),
  {ok, Filter14} = bloom:deserialize(Serialized),
  ok = bloom:clear(Filter1),
  false = bloom:check(Filter1, Key),
  true = bloom:check(Filter13, Key),
  true = bloom:check(Filter14, Key),
  ok.

set(_) ->
  {ok, Ref} = bloom:new(10, 80),
  Key = "binkeyfortest",
  ok = bloom:set(Ref, Key),
  ok.

check(_) ->
  Key = "binkeyfortest",
  {ok, Ref} = bloom:new(10, 80),
  false = bloom:check(Ref, Key),
  ok = bloom:set(Ref, Key),
  true = bloom:check(Ref, Key),
  false = bloom:check(Ref, "unknown_key"),
  %% we can check a serialized bloom
  {ok, Serialized} = bloom:serialize(Ref),
  true = is_binary(Serialized),
  true = bloom:check(Serialized, Key),
  ok.

check_and_set(_) ->
  Key = "binkeyfortest",
  {ok, Ref} = bloom:new(10, 80),
  false = bloom:check_and_set(Ref, Key),
  true = bloom:check(Ref, Key),
  true = bloom:check_and_set(Ref, Key),
  bloom:clear(Ref),
  false = bloom:check(Ref, Key),
  false = bloom:check_and_set(Ref, Key),
  ok.

check_serialized(_) ->
  Key = "some_key_to_check",
  {ok, Ref} = bloom:new(10, 80),
  ok = bloom:set(Ref, Key),
  {ok, SerializedFilter} = bloom:serialize(Ref),
  true = is_binary(SerializedFilter),
  true = bloom:check(Ref, Key),
  false = bloom:check(Ref, "unknown_key"),
  true = bloom:check(SerializedFilter, Key),
  false = bloom:check(SerializedFilter, "unknown_key"),
  ok.

clear(_) ->
  Key = "binkeyfortest",
  {ok, Ref} = bloom:new(10, 80),
  ok = bloom:set(Ref, Key),
  true = bloom:check(Ref, Key),
  ok = bloom:clear(Ref),
  false = bloom:check(Ref, Key),
  ok.

%% =============================================================================
%% group: perf
%% =============================================================================
perf_sequential_csc(_) ->
  R = perftest:sequential(1000,
    fun() ->
      {ok, Ref} = bloom:new(10, 80),
      Key = uuid:uuid_to_string(uuid:get_v4()),
      ok = bloom:set(Ref, Key),
      true = bloom:check(Ref, Key)
    end),
  true = R > 3000,
  ok.

perf_sequential_csc_large(_) ->
  {ok, Ref} = bloom:new(9585059, 1000000),
  R = perftest:sequential(100,
    fun() ->
      Key = uuid:uuid_to_string(uuid:get_v4()),
      ok = bloom:set(Ref, Key),
      true = bloom:check(Ref, Key),
      ok
    end),
  true = R > 3000,
  ok.

perf_parallel_read(_) ->
  Key = uuid:uuid_to_string(uuid:get_v4()),
  {ok, Ref} = bloom:new(10, 80),
  ok = bloom:set(Ref, Key),
  R = perftest:comprehensive(1000,
    fun() ->
      true = bloom:check(Ref, Key)
    end),
  true = lists:all(fun(E) -> E >= 3500 end, R),
  ok.

%% =============================================================================
%% helpers
%% =============================================================================
