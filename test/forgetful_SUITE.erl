-module(forgetful_SUITE).
-compile(export_all).
-import(ct_helper, [config/2]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    {group, forgetful}
  ].

groups() ->
  [

    {forgetful,
      [parallel, shuffle],
      [new, serialize, deserialize, set_and_check, clear]}
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
  {ok, Ref} = bloom:new_forgetful(50, 80, 3, 1),
  true = is_reference(Ref),
  1 = bloom:type(Ref),
  ok.

serialize(_) ->
  {ok, Ref} = bloom:new_forgetful(50, 80, 3, 1),
  {ok, SerializedFilter} = bloom:serialize(Ref),
  true = is_binary(SerializedFilter),
  ok.

deserialize(_) ->
  {ok, Filter1} = bloom:new_forgetful(50, 80, 3, 1),
  {ok, _} = bloom:serialize(Filter1),
  Key = "test_key",
  false = bloom:check(Filter1, Key),
  false = bloom:set(Filter1, Key),
  true = bloom:set(Filter1, Key),
  true = bloom:check(Filter1, Key),
  {ok, Serialized} = bloom:serialize(Filter1),
  true = is_binary(Serialized),
  {ok, Filter13} = bloom:deserialize(Serialized),
  1 = bloom:type(Filter13),
  {ok, Filter14} = bloom:deserialize(Serialized),
  ok = bloom:clear(Filter1),
  false = bloom:check(Filter1, Key),
  true = bloom:check(Filter13, Key),
  true = bloom:check(Filter14, Key),
  ok.

set_and_check(_) ->
  {ok, Ref} = bloom:new_forgetful(50, 80, 3, 1),
  Key1 = <<"k1">>,
  Key2 = <<"k2">>,
  Key3 = <<"k3">>,
  Key4 = <<"k4">>,
  Key5 = <<"k5">>,
  Key6 = <<"k6">>,
  Keys = [Key1, Key2, Key3, Key4, Key5, Key6],
  [] = [K || K <- Keys, bloom:check(Ref, K)],
  false = bloom:set(Ref, Key1),
  ?assertEqual([Key1], [K || K <- Keys, bloom:check(Ref, K)]),
  false = bloom:set(Ref, Key2),
  ?assertEqual([Key1, Key2], [K || K <- Keys, bloom:check(Ref, K)]),
  false = bloom:set(Ref, Key3),
  ?assertEqual([Key1, Key2, Key3], [K || K <- Keys, bloom:check(Ref, K)]),
  false = bloom:set(Ref, Key4),
  ?assertEqual([Key2, Key3, Key4], [K || K <- Keys, bloom:check(Ref, K)]),
  false = bloom:set(Ref, Key5),
  ?assertEqual([Key3, Key4, Key5], [K || K <- Keys, bloom:check(Ref, K)]),
  false = bloom:set(Ref, Key6),
  ?assertEqual([Key4, Key5, Key6], [K || K <- Keys, bloom:check(Ref, K)]),
  ok.

clear(_) ->
  Key = "binkeyfortest",
  {ok, Ref} = bloom:new_forgetful(50, 80, 3, 1),
  false = bloom:set(Ref, Key),
  true = bloom:set(Ref, Key),
  true = bloom:check(Ref, Key),
  ok = bloom:clear(Ref),
  false = bloom:check(Ref, Key),
  ok.

%% =============================================================================
%% helpers
%% =============================================================================