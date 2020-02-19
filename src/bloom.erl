
%% @doc This is a NIF wrappper around [https://crates.io/crates/bloomfilter], a simple but fast Bloom filter implementation, that requires only 2 hash functions, generated with SipHash-1-3 using randomized keys.

-module(bloom).
%% API
-export([new/2,
         new_for_fp_rate/2,
         serialize/1,
         deserialize/1,
         deserialize/7,
         to_bin/1,
         from_bin/1,
         set/2,
         check/2,
         check_and_set/2,
         clear/1
         ]).

%% Native library support
-export([load/0]).
-on_load(load/0).

%% rev this if the internal structure of the bloom filter changes
-define(ERBLOOM_VERSION1, 1).

-type sip_keys() :: {non_neg_integer(), non_neg_integer()}.

-type serialized_bloom() :: {Bitmap :: binary(), NumBits :: pos_integer(), NumFuns :: pos_integer(), sip_keys(), sip_keys()}.

-opaque bloom() :: reference().

-export_type([bloom/0]).

%% @doc Create a new bloom filter structure. `BitmapSize' is the size in bytes (not bits) that will be allocated in memory `ItemsCount' is an estimation of the maximum number of items to store.
-spec new(BitmapSize :: pos_integer(), ItemsCount :: pos_integer()) -> {ok, Bloom :: bloom()}.
new(_BitmapSize, _ItemsCount) ->
    not_loaded(?LINE).

%% @doc Create a new bloom filter structure. `ItemsCount' is an estimation of the maximum number of items to store. `FalsePositiveRate' is the wanted rate of false positives, in ]0.0, 1.0[.
-spec new_for_fp_rate(ItemsCount :: pos_integer(), FalsePositiveRate :: float()) -> {ok, Bloom :: bloom()}.
new_for_fp_rate(_ItemsCount, _FP_Rate) ->
    not_loaded(?LINE).

%% @doc Serialize a bloom filter to Erlang terms. `check/2' can be used against this serialized form efficently.
-spec serialize(Bloom :: bloom()) -> {ok, serialized_bloom()}.
serialize(_Ref) ->
    not_loaded(?LINE).

%% @doc Serialize a bloom filter and encode it as a versioned binary. `check/2' can be used against this binary form efficently.
-spec to_bin(bloom()) -> binary().
to_bin(Ref) ->
    {ok, {Bitmap,NumBits,NumFuns,{Sv00,Sv01},{Sv10,Sv11}}} = serialize(Ref),
    <<?ERBLOOM_VERSION1:8/integer, NumBits:64/integer-unsigned-little, NumFuns:32/integer-unsigned-little,
      Sv00:64/integer-unsigned-little, Sv01:64/integer-unsigned-little,
      Sv10:64/integer-unsigned-little, Sv11:64/integer-unsigned-little, Bitmap/binary>>.

%% @doc Deserialize a versioned binary into a bloom filter reference.
-spec from_bin(binary()) -> bloom().
from_bin(<<?ERBLOOM_VERSION1:8/integer, NumBits:64/integer-unsigned-little, NumFuns:32/integer-unsigned-little,
      Sv00:64/integer-unsigned-little, Sv01:64/integer-unsigned-little,
      Sv10:64/integer-unsigned-little, Sv11:64/integer-unsigned-little, Bitmap/binary>>) ->
    deserialize(Bitmap, NumBits, NumFuns, Sv00, Sv01, Sv10, Sv11).

%% @doc Deserialize a previously serialized bloom filter back into a bloom filter reference.
-spec deserialize(serialized_bloom()) -> {ok, bloom()}.
deserialize({Bitmap,NumBits,NumFuns,{Sv00,Sv01},{Sv10,Sv11}}) ->
    deserialize(Bitmap, NumBits, NumFuns, Sv00, Sv01, Sv10, Sv11).

%% @doc Deserialize a previously serialized bloom filter back into a bloom filter reference.
deserialize(_Bitmap, _NumBits, _NumFuns, _Sv00, _Sv01, _Sv10, _Sv11) ->
    not_loaded(?LINE).

%% @doc Record the presence of `Key' in `Bloom'.
-spec set(Bloom :: bloom(), Key :: term()) -> ok.
set(_Ref, _Key) ->
    not_loaded(?LINE).

%% @doc Check for the presence of `Key' in `Bloom'.
%% Serialized and binary encoded bloom filters can be used with this
%% function when you wish to check for the key and do not need to use set
%% (eg. a static bloom filter stored in a database).
-spec check(bloom() | serialized_bloom() | binary(), term()) -> boolean().
check(Bloom, Key) when is_reference(Bloom) ->
    check_nif(Bloom, Key);
check(<<?ERBLOOM_VERSION1:8/integer, NumBits:64/integer-unsigned-little, NumFuns:32/integer-unsigned-little,
      Sv00:64/integer-unsigned-little, Sv01:64/integer-unsigned-little,
      Sv10:64/integer-unsigned-little, Sv11:64/integer-unsigned-little, Bitmap/binary>>, Key) ->
    check_nif(Bitmap, NumBits, NumFuns, Sv00, Sv01, Sv10, Sv11, Key);
check({Bitmap,NumBits,NumFuns,{Sv00,Sv01},{Sv10,Sv11}}, Key) ->
    check_nif(Bitmap, NumBits, NumFuns, Sv00, Sv01, Sv10, Sv11, Key).

%% @doc Record the presence of `Key' in `Bloom' and return whether it was present before.
-spec check_and_set(Bloom :: bloom(), Key :: term()) -> boolean().
check_and_set(_Ref, _Key) ->
    not_loaded(?LINE).

%% @doc Clear all of the bits in the filter, removing all keys from the set.
-spec clear(Bloom :: bloom()) -> ok.
clear(_Ref) ->
    not_loaded(?LINE).

check_nif(_Ref, _Key) ->
    not_loaded(?LINE).

check_nif(_Bitmap, _NumBits, _NumFuns, _Sv00, _Sv01, _Sv10, _Sv11, _Key) ->
    not_loaded(?LINE).

%% @private
load() ->
    erlang:load_nif(filename:join(priv(), "libbloom"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv()->
  case code:priv_dir(?MODULE) of
      {error, _} ->
          EbinDir = filename:dirname(code:which(?MODULE)),
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv");
      Path ->
          Path
  end.
