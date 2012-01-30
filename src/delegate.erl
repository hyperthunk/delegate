%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2012 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(delegate).
-include_lib("annotations/include/types.hrl").
-export([codegen/3, delegate_advice/4]).

codegen(A=#annotation{data=Data}, _Mod, AST) ->
    io:format("Annotation Data: ~p~n",[lists:keyfind(arity, 1, Data)]),
    case lists:keyfind(delegate, 1, Data) of
        {delegate, Delegates} when is_list(Delegates) ->
            %% NB: you need a *little* understanding of erl_syntax here
            {_FN, FA} = erl_syntax_lib:analyze_function(AST),
            Arity = case lists:keyfind(arity, 1, Data) of
                {arity, N} when is_integer(N) -> N;
                _ -> FA
            end,
            [ build_spec(D, Arity, A) || D <- Delegates ];
        Other ->
            %% TODO: clearer error handling API
            io:format("Other: ~p~n", [Other]),
            {error, "no delegates defined"}
    end.

delegate_advice(A, M, F, Inputs) ->
    Argv = make_args(A, M, F, Inputs),
    erlang:apply(M, F, Argv).

make_args(A=#annotation{data=Data}, M, F, Inputs) ->
    Arity = length(Inputs),
    InputBindings =
        lists:zip([ varname(I) || I <- lists:seq(1, Arity) ], Inputs),
    {target, Target} = lists:keyfind(target, 1, Data),
    Ctx = context([{'$Xa',  A},
                   {'$Xd',  Data},
                   {'$M',   M},
                   {'$F',   F},
                   {'$A',   Arity},
                   {'$T',   Target},
                   {'$I',   Inputs}] ++ InputBindings),
    {args, Spec} = lists:keyfind(args, 1, Data),
    case walk(Spec, Ctx) of
        L when is_list(L) ->
            L;
        Term ->
            [Term]
    end.

context(L) ->
    dict:from_list(L).

varname(I) ->
    list_to_atom("$" ++ integer_to_list(I)).

walk(Spec, Ctx) when is_tuple(Spec) ->
    {_, _, Acc} = lists:foldl(fun do_walk/2, 
                                {tuple, Ctx, Spec}, lists:seq(1, size(Spec))),
    Acc;
walk(Spec, Ctx) when is_list(Spec) ->
    {_, _, _, Acc} = lists:foldl(fun do_walk/2, {list, Ctx, 0, []}, Spec),
    lists:reverse(Acc);
walk(Spec, Ctx) when is_atom(Spec) ->
    lookup(Spec, Ctx, -1);
walk(Other, _) ->
    Other.

do_walk(Index, {tuple, Ctx, Acc}) ->
    NewVal = setelement(Index, Acc, 
        walk(lookup(element(Index, Acc), Ctx, Index), Ctx)),
    {tuple, Ctx, NewVal};
do_walk('$I'=E, {list, Ctx, Idx, Acc}) ->
    {list, Ctx, Idx + 1, lists:reverse(lookup(E, Ctx, Idx)) ++ Acc};
do_walk(['$I'=E], {list, Ctx, Idx, Acc}) ->
    {list, Ctx, Idx + 1, [[lookup(E, Ctx, Idx)]|Acc]};
do_walk(E, {list, Ctx, Idx, Acc}) ->
    V = lookup(E, Ctx, Idx),
    {list, Ctx, Idx + 1, [walk(V, Ctx)|Acc]}.

lookup('$REST', Ctx, Idx) ->
    Args = lookup('$I', Ctx, Idx),
    slice(Args, Idx);
lookup(Elem, Ctx, _Idx) ->
    case dict:find(Elem, Ctx) of
        error ->
            Elem;
        {ok, Val} ->
            Val
    end.

slice(Args, Idx) ->
    lists:sublist(Args, Idx, length(Args) - Idx).

build_spec(Name, Arity, A=#annotation{ data=Data }) ->
    Delegate = list_to_atom(Name),
    {delegate_advice, Delegate, Arity,
        A#annotation{data=[{target, Delegate}|Data]}}.
