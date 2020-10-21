%% @doc
%% This is a NIF wrapper around [https://crates.io/crates/bloomfilter],
%% a simple but fast Bloom filter implementation, that requires only 2 hash functions,
%% generated with SipHash-1-3 using randomized keys.
%% @end

-module(bloom_nif).
%% API
-export([
  new/1,
  serialize/1,
  deserialize/1,
  set/2,
  check/2,
  check_and_set/2,
  clear/1,
  ftype/1
]).

%% Native library support
-export([load/0]).
-on_load(load/0).

%% rev this if the internal structure of the bloom filter changes
-define(ERBLOOM_VERSION1, 1).

-type serialized_bloom() :: binary().
-opaque bloom() :: reference().
-export_type([bloom/0, serialized_bloom/0]).

%% @doc Create a new filter structure.
-spec new(_Opts :: map()) -> {ok, Bloom :: bloom()} | {error, Reason :: binary()}.
new(_Opts) ->
  not_loaded(?LINE).

%% @doc Get type of filter
-spec ftype(_Ref :: bloom()) -> number() | {error, Reason :: binary()}.
ftype(_Ref) ->
  not_loaded(?LINE).

%% @doc Serialize a bloom filter to Erlang terms. `check/2' can be used against this serialized form efficently.
-spec serialize(Bloom :: bloom()) -> {ok, serialized_bloom()}.
serialize(_Ref) ->
  not_loaded(?LINE).

%% @doc Deserialize a previously serialized bloom filter back into a bloom filter reference.
-spec deserialize(serialized_bloom()) -> {ok, bloom()}.
deserialize(_SerializedBloom) ->
  not_loaded(?LINE).

%% @doc Record the presence of `Key' in `Bloom'.
-spec set(Bloom :: bloom(), Key :: term()) -> ok.
set(_Ref, _Key) ->
  not_loaded(?LINE).

%% @doc Check for the presence of `Key' in `Bloom'.
%% Serialized and binary encoded bloom filters can be used with this
%% function when you wish to check for the key and do not need to use set
%% (eg. a static bloom filter stored in a database).
-spec check(bloom() | serialized_bloom(), term()) -> boolean().
check(SerializedBloom, Key) when is_binary(SerializedBloom) ->
  check_serialized(SerializedBloom, Key);
check(Bloom, Key) ->
  vcheck(Bloom, Key).
vcheck(_Bloom, _Key) when is_reference(_Bloom) ->
  not_loaded(?LINE).

%% @doc Record the presence of `Key' in `Bloom' and return whether it was present before.
-spec check_and_set(Bloom :: bloom(), Key :: term()) -> boolean().
check_and_set(_Ref, _Key) ->
  not_loaded(?LINE).

%% @doc Clear all of the bits in the filter, removing all keys from the set.
-spec clear(Bloom :: bloom()) -> ok.
clear(_Ref) ->
  not_loaded(?LINE).

-spec check_serialized(Bloom :: bloom(), Key :: term()) -> boolean().
check_serialized(_Ref, _Key) ->
  not_loaded(?LINE).

%% @private
load() ->
  erlang:load_nif(filename:join(priv(), "libbloom"), none).

not_loaded(Line) ->
  erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
  case code:priv_dir(?MODULE) of
    {error, _} ->
      EbinDir = filename:dirname(code:which(?MODULE)),
      AppPath = filename:dirname(EbinDir),
      filename:join(AppPath, "priv");
    Path ->
      Path
  end.
