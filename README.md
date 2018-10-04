rebar_mix
=====

A rebar plugin for building Elixir dependencies with mix.

Use
---

Add the plugin to your rebar config:

``` erlang
{plugins, [rebar_mix]}.
{provider_hooks, [{post, [{compile, {mix, consolidate_protocols}}]}]}.
```    

The `consolidate_protocols` hook places beams in `_build/<profile>/consolidated` that will need to be included in a release when built. Using:


``` erlang
{overlay, [{copy, "{{base_dir}}/consolidated", "releases/{{release_version}}/consolidated"}]}
```

And update your `vm.args.src` to include:

``` erlang
-pa releases/${REL_VSN}/consolidated
```

