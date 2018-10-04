rebar_mix
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar_mix, ".*", {git, "git@host:user/rebar_mix.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar_mix
    ===> Fetching rebar_mix
    ===> Compiling rebar_mix
    <Plugin Output>
