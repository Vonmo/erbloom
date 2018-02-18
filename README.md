# erbloom
Safe and Fast Bloom Filter for Erlang

Without docker you must install erlang 20.1 and rust 1.23 on your machine. After you can run these goals:

**release:**
`rebar3 as prod release`

**test:**
`rebar3 as test ct`

In Docker enviroment:
* `make build_imgs` - build docker images
* `make up` - run sandbox
* `make down` - terminate sandbox
* `make tests` - run tests
* `make lint` - linter
* `make xref` - xref analysis
* `make prod` - generate release for target
