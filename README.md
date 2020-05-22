# erbloom
Safe and Fast Bloom Filter for Erlang

## Features:
* [Bloom filter structure](https://en.wikipedia.org/wiki/Bloom_filter) (type: `bloom`)
* [Forgetful Bloom Filters](http://dprg.cs.uiuc.edu/docs/fbf_cac15/fbfpaper-2.pdf) (type: `fbf`)

## Using as a lib
1. Add deps in rebar.conf:
  ```
  {deps, [
      {erbloom, ".*", {git, "https://github.com/Vonmo/erbloom.git", {tag, "v2.0.0"}}}      
  ]}
  ```
2. Now you can create a new filter instance:
  `{ok, Filter} = bloom:new(9585059,1000000).`
   or filter with wanted rate of false positives: 
   `bloom:new_optimal(1000000, 0.55).`
3. Create a new forgetful filter:
   `{ok, Filter} = bloom:new_forgetful(BitmapSize, ItemsCount, NumFilters, RotateAfter).`
   or with fp_rate:
   `bloom:new_forgetful(ItemsCount, NumFilters, RotateAfter, FpRate).`
3. Set a new element
  `bloom:set(Filter, "somekey").`
4. Check up element
  `bloom:check(Filter, "anotherkey").`
5. Serialize
   `{ok, Binary} = bloom:serialize(Filter).`
6. Deserialize
   `bloom:deserialize(Binary).`

## Docker environment
* `make build_imgs` - build docker images
* `make up` - run sandbox
* `make down` - terminate sandbox
* `make tests` - run tests
* `make lint` - linter
* `make xref` - xref analysis
* `make prod` - generate release for target

##
Without docker you must install erlang >=20.1 and rust >=1.23 on your machine. After you can run these goals:
**release:**
`rebar3 as prod release`

**test:**
`rebar3 as test ct`