## rebar_elixir_plugin

This plugin allows you to compile Elixir files in your Erlang projects using Rebar.

## Installation

To install this plugin, you need to add it as a dependency to your rebar application:

    {deps, [{ rebar_elixir_plugin, ".*",
             {git, "git://github.com/yrashk/rebar_elixir_plugin"}}]}.
    
    %% Let rebar know about the new plugins
    {plugins, [rebar_elixir_compiler, rebar_exunit] }.

This plugin depends on Elixir too, so you need to add it as a rebar dependency:

    {deps, [{ elixir, "0.9.*",
             {git, "git://github.com/elixir-lang/elixir"}}]}.

Elixir is structured similarly to Erlang's OTP. It is divided into applications that are placed inside the `lib` directory, as seen on its [source code repository](https://github.com/elixir-lang/elixir). We need to explicitly tell rebar so:

    {lib_dirs, [
      "deps/elixir/lib"
    ]}.

This should be all.

## Available configuration

* `ex_first_files` - first elixir files to be configured;
* `src_dirs` - where to find elixir source;
* `ex_opts` - elixir compilation options: `ignore_module_conflict` (default to true), `docs` (default to false) and `debug_info` (default to false);

## License

Copyright 2012 Yurii Rashkovskii

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
