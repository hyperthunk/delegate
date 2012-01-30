# Delegate - Build-Time Delegate/Wrapper Function Code-gen

This library builds on 
[annotations](https://github.com/hyperthunk/annotations) to provide the 
ability to generate wrapper/delegate functions dynamically, based on annotated
targets.

## Basic Usage

Take a basic example of boilerplate wrapper functions:

```erlang
info(Msg, Args) ->
    log(info, Msg, Args).

warn(Msg, Args) ->
    warn(info, Msg, Args).

error(Msg, Args) ->
    error(info, Msg, Args).

log(Level, Message, Args) ->
    case erlang:get({simple_log, loglevel}) of
      Level -> io:format(Message, Args);
      _ -> ok
    end.
```

Instead of writing lots of wrapper functions by hand, we can *generate* these
at compile time and have `delegate` forward the required arguments to the 
base `log/3` function on our behalf.

```erlang
-module(simple_log).
-export([log/3]).
-compile({no_auto_import, [error/2]}).
-include_lib("annotations/include/annotations.hrl").

-delegate([{delegate, ["info", "warn", "error"]},
           {args, ['$T', '$I']},
           {arity, 2}]).
log(Level, Message, Args) ->
    case erlang:get({?MODULE, loglevel}) of
        Level ->
            io:format(Message, Args);
        _ ->
            ok
    end.
```

The `delegate` annotation takes a list of properties, whose elements have the
following meaning:

1. `delegate` contains a list of function *names* that you wish to generate
2. `arity` specifies the arity you wish the exported function(s) to have
3. `args` contains a *specification* for the input arguments you wish to forward to the target (i.e., annotated) function

We will look at the `args` specification format in more detail later on, but
for now we need to know that `$T` refers to the *target/annotated* function
and that `$I` refers to the complete list of input arguments to the wrapper
function. After build/processing, this application of the `delegate` attribute
will provide the following modifications to the `simple_log` module:

```erlang
-annotation({annotation, delegate,
	     {function, {simple_log, log, 3}},
	     [{delegate, ["info", "warn", "error"]},
	      {args, ['$T', '$I']}, {arity, 2}]}).

-export([info/2]).

-export([warn/2]).

-export([error/2]).

info(V73, V45) ->
    delegate:delegate_advice({annotation, delegate,
			      {function, {simple_log, log, 3}},
			      [{target, info},
			       {delegate, ["info", "warn", "error"]},
			       {args, ['$T', '$I']}, {arity, 2}]},
			     simple_log, log, [V73, V45]).

warn(V51, V95) ->
    delegate:delegate_advice({annotation, delegate,
			      {function, {simple_log, log, 3}},
			      [{target, warn},
			       {delegate, ["info", "warn", "error"]},
			       {args, ['$T', '$I']}, {arity, 2}]},
			     simple_log, log, [V51, V95]).

error(V60, V32) ->
    delegate:delegate_advice({annotation, delegate,
			      {function, {simple_log, log, 3}},
			      [{target, error},
			       {delegate, ["info", "warn", "error"]},
			       {args, ['$T', '$I']}, {arity, 2}]},
			     simple_log, log, [V60, V32]).

log(Level, Message, Args) ->
    case erlang:get({simple_log, loglevel}) of
      Level -> io:format(Message, Args);
      _ -> ok
    end.
```

The `delegate_advice` implementation simply forwards the call to your 
target (`log/3`) function with the arguments formatted as per your 
specification.

Another example of this pattern can be seen in the base function `bin_op`
(taken from the [ogql](https://github.com/hyperthunk/ogql) library), which has
numerous wrappers for specific operator names:

```erlang
-delegate([{args, ['$T', '$I']},
           {arity, 2},
           {delegate, [
                "eq", "gt",
                "gteq", "lt",
                "lteq", "like", 
                "contains", "starts_with",
                "ends_with", "matches", "path_exists"
            ]}]).
binop(Op, Axis, {{_,_,_}, _}=Literal) ->
    binop(Op, Axis, {literal, Literal});
binop(Op, Axis, Literal) when is_integer(Literal) orelse
                              is_float(Literal) orelse
                              is_list(Literal) orelse
                              is_record(Literal, semver) ->
    binop(Op, Axis, {literal, Literal});
binop(Op, Axis, {literal, _}=Literal) ->
    {Axis, {operator, Op}, Literal}.
```

## Controlling the arguments passed to the target function

The following `atoms` have special meaning when used in an argument spec:

- `$Xa` The `annotation` record passed to the codegen call
- `$Xd` The `data` field from the `annotation` record
- `$M`  The module in which the `annotation` was applied
- `$F`  The name of the generated function currently executing
- `$A`  The arity of the generated function currently executing
- `$T`  The name of the target (annotated) function
- `$I`  The complete set of input arguments to the generated function

In addition to these, the atoms `[$0, $1 .. $N]` refer to each input argument
in position. If you refer to an input argument whose index is greater than or
equal to the arity of the generated function, then codegen will fail.

If you specify the `$I` input arguments by themselves, then will be 
concatenated with the rest of the specification, such that `[$T, $I]` will
produce the following arguments when `$T` resolves to `foo` and `$I` resolves
to `["Hello ~p~n", [world]]`: `[foo, "Hello ~p~n", [world]]`.

If you want to have the input arguments provided as a single (list) input, 
then you must specify it like so: `[$T, [$I]]`: `[foo, ["Hello ~p~n", [world]]]`.

You can also supply a tuple instead of a list, in which case the tuple may
contain the same *special* atoms in any of its fields:

    {args, {call, {'$M', '$T', ['$I']}}}

The `call` atom is treated as a literal, whereas the other elements are 
resolved prior to forwarding the call to your target/annotated function.

You can also combine the use of lists and tuples in specifying the way in 
which you want the input arguments transformed prior to delegation taking
place.

## License

This work is distributed under a permissive BSD-style license.

## Versioning

This project will adhere to the principles of
[semantic versioning](http://semver.org) once a first public API is declared.
