%% @doc This is an implementation of Forgetful Bloom Filters [http://dprg.cs.uiuc.edu/docs/fbf_cac15/fbfpaper-2.pdf] built on top of [https://crates.io/crates/bloomfilter]

-module(forgetful_bloom).
%% API
-export([new/4,
         new_for_fp_rate/4,
         set/2,
         check/2,
         clear/1
         ]).


-opaque bloom() :: reference().
-export_type([bloom/0]).

%% @doc Create a new forgetful bloom filter structure. `BitmapSize' is the size in bytes (not bits) that will be allocated in memory `ItemsCount' is an estimation of the maximum number of items to store, `NumFilters' is the number of filters to maintain (minimum of 3) and `RotateAfter' is how many insertions to do into a filter before rotating a blank filter into the `future' position.
-spec new(BitmapSize :: pos_integer(), ItemsCount :: pos_integer(), NumFilters :: pos_integer(), RotateAfter :: pos_integer()) -> {ok, Bloom :: bloom()}.
new(BitmapSize, ItemsCount, NumFilters, RotateAfter) when NumFilters > 2 ->
    bloom:new_forgetful(BitmapSize, ItemsCount, NumFilters, RotateAfter).

%% @doc Create a new forgetful bloom filter structure. `ItemsCount' is an estimation of the maximum number of items to store. `FalsePositiveRate' is the wanted rate of false positives, in ]0.0, 1.0[, `NumFilters' is the number of filters to maintain (minimum of 3) and `RotateAfter' is how many insertions to do into a filter before rotating a blank filter into the `future' position.
-spec new_for_fp_rate(ItemsCount :: pos_integer(), FalsePositiveRate :: float(), NumFilters :: pos_integer(), RotateAfter :: pos_integer()) -> {ok, Bloom :: bloom()}.
new_for_fp_rate(ItemsCount, FP_Rate, NumFilters, RotateAfter) ->
    bloom:new_forgetful_for_fp_rate(ItemsCount, FP_Rate, NumFilters, RotateAfter).

%% @doc Record the presence of `Key' in `ForgetfulBloom'. Like `bloom:check_and_set/2' a boolean is returned to indicate if the value was already present.
%% @see bloom:check_and_set/2
-spec set(ForgetfulBloom :: bloom(), Key :: term()) -> WasAlreadyPresent :: boolean().
set(Ref, Key) ->
    bloom:set_forgetful(Ref, Key).

%% @doc Check for the presence of `Key' in `ForgetfulBloom'.
-spec check(ForgetfulBloom :: bloom(), term()) -> boolean().
check(Ref, Key) ->
    bloom:check_forgetful(Ref, Key).

%% @doc Clear all of the bits in the filter, removing all keys from the set.
-spec clear(ForgetfulBloom :: bloom()) -> ok.
clear(Ref) ->
    bloom:clear_forgetful(Ref).



