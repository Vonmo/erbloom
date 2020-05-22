-module(bloom).

-export([
  new/2,
  new_optimal/2,
  new_forgetful/4,
  new_forgetful_optimal/4,
  set/2,
  check/2,
  check_and_set/2,
  clear/1,
  type/1,
  serialize/1,
  deserialize/1
  ]).

%% @doc Create a new bloom filter structure.
%% `BitmapSize` is the size in bytes (not bits) that will be allocated in memory
%% `ItemsCount` is an estimation of the maximum number of items to store.
-spec new(BitmapSize :: pos_integer(), ItemsCount :: pos_integer()) -> {ok, Bloom :: bloom_nif:bloom()}.
new(BitmapSize, ItemsCount) ->
  bloom_nif:new(#{
    filter_type => bloom,
    bitmap_size => BitmapSize,
    items_count => ItemsCount
  }).

%% @doc Create a new bloom filter structure.
%% `ItemsCount` is an estimation of the maximum number of items to store.
%% `FalsePositiveRate` is the wanted rate of false positives, in [0.0, 1.0].
-spec new_optimal(ItemsCount :: pos_integer(), FalsePositiveRate :: float()) -> {ok, Bloom :: bloom_nif:bloom()}.
new_optimal(ItemsCount, FalsePositiveRate) when FalsePositiveRate >= 0.0 andalso FalsePositiveRate =< 1.0 ->
  bloom_nif:new(#{
    filter_type => bloom,
    items_count => ItemsCount,
    fp_rate => FalsePositiveRate
  }).

%% @doc Create a new forgetful bloom filter structure.
%% `BitmapSize` is the size in bytes (not bits) that will be allocated in memory
%% `ItemsCount` is an estimation of the maximum number of items to store,
%% `NumFilters` is the number of filters to maintain (minimum of 3) and
%% `RotateAfter` is how many insertions to do into a filter before rotating a blank filter into the `future' position.
-spec new_forgetful(BitmapSize :: pos_integer(), ItemsCount :: pos_integer(), NumFilters :: pos_integer(), RotateAfter :: pos_integer())
      -> {ok, Bloom :: bloom_nif:bloom()}.
new_forgetful(BitmapSize, ItemsCount, NumFilters, RotateAfter) when NumFilters > 2 ->
  bloom_nif:new(#{
    filter_type => fbf,
    bitmap_size => BitmapSize,
    items_count => ItemsCount,
    capacity => NumFilters,
    rotate_at => RotateAfter
  }).

%% @doc Create a new forgetful bloom filter structure.
%% `BitmapSize` is the size in bytes (not bits) that will be allocated in memory
%% `ItemsCount` is an estimation of the maximum number of items to store,
%% `NumFilters` is the number of filters to maintain (minimum of 3) and
%% `RotateAfter` is how many insertions to do into a filter before rotating a blank filter into the `future' position.
%% `FalsePositiveRate` is the wanted rate of false positives, in [0.0, 1.0].
-spec new_forgetful_optimal(ItemsCount :: pos_integer(), NumFilters :: pos_integer(), RotateAfter :: pos_integer(), FalsePositiveRate :: float())
      -> {ok, Bloom :: bloom_nif:bloom()}.
new_forgetful_optimal(ItemsCount, NumFilters, RotateAfter, FalsePositiveRate) when NumFilters > 2 andalso FalsePositiveRate >= 0.0 andalso FalsePositiveRate =< 1.0 ->
  bloom_nif:new(#{
    filter_type => fbf,
    items_count => ItemsCount,
    capacity => NumFilters,
    rotate_at => RotateAfter,
    fp_rate => FalsePositiveRate
  }).


%% @doc Record the presence of `Key` in `Bloom` and `ForgetfulBloom`
%% For `ForgetfulBloom` a boolean is returned to indicate if the value was already present (like `check_and_set/2`).
-spec set(Bloom :: bloom_nif:bloom(), Key :: term()) -> ok | boolean().
set(Bloom, Key) ->
  bloom_nif:set(Bloom, Key).

%% @doc Check for the presence of `Key` in `Bloom`.
%% Serialized and binary encoded bloom filters can be used with this
%% function when you wish to check for the key and do not need to use set
%% (eg. a static bloom filter stored in a database).
-spec check(Bloom :: bloom_nif:bloom() | bloom_nif:serialized_bloom(), Key :: term()) -> boolean().
check(Bloom, Key) ->
  bloom_nif:check(Bloom, Key).

%% @doc Record the presence of `Key` in `Bloom` or `ForgetfulBloom`
%% and return whether it was present before.
-spec check_and_set(Bloom :: bloom_nif:bloom(), Key :: term()) -> boolean().
check_and_set(Bloom, Key) ->
  bloom_nif:check_and_set(Bloom, Key).

%% @doc Clear all of the bits in the filter, removing all keys from the set.
-spec clear(Bloom :: bloom_nif:bloom()) -> ok.
clear(Bloom) ->
  bloom_nif:clear(Bloom).

%% @doc Get type of filter
-spec type(Bloom :: bloom_nif:bloom()) -> number() | {error, Reason :: binary()}.
type(Bloom) ->
  bloom_nif:ftype(Bloom).

%% @doc Serialize a bloom filter to binary.
%% `check/2' can be used against this serialized form efficiently.
-spec serialize(Bloom :: bloom_nif:bloom()) -> {ok, bloom_nif:serialized_bloom()}.
serialize(Bloom) ->
  bloom_nif:serialize(Bloom).

%% @doc Deserialize a previously serialized bloom filter back
%% into a bloom filter reference.
-spec deserialize(SerializedBloom :: bloom_nif:serialized_bloom()) -> {ok, bloom_nif:bloom()}.
deserialize(SerializedBloom) ->
  bloom_nif:deserialize(SerializedBloom).