rebar_mix
=====

A rebar plugin for building Elixir dependencies with mix.

Requirements:

* rebar3 3.7.0 or above
* Elixir 1.7.4 or above, unless your Elixir dependencies have no transitive compile time dependencies in which case any version starting with 1.7.0 may work.
* `mix local.hex`, while mix hex isn't used, mix checks for it and will block with a message `Could not find Hex, which is needed to build dependency ...` if it isn't found. Running `mix local.hex` will install mix's hex script archive and make the check happy.

Use
---

Add the plugin to your rebar config:

``` erlang
{plugins, [rebar_mix]}.
{provider_hooks, [
  {post, [{compile, {mix, consolidate_protocols}}]}
]}.
```

The `consolidate_protocols` hook places beams in `_build/<profile>/consolidated` that will need to be included in a release when built. Using:


``` erlang
{overlay, [{copy, "{{base_dir}}/consolidated", "releases/{{release_version}}/consolidated"}]}
```

And update your `vm.args.src` to include:

``` erlang
-pa releases/${REL_VSN}/consolidated
```
