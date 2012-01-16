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
-export([codegen/3, codegen_advised/4]).

codegen(A=#annotation{name=N, data=Data}, _Mod, AST) ->
    case lists:keyfind(N, 1, Data) of
        Delegates when is_list(Delegates) ->
            %% NB: this *can* return either {gen_function, Name}, in which case
            %% we generate a function that calls ?MODULE:codegen_advised/4 - or -
            %% you return {add_function, Name, codegen:gen_function/2/3} in which case
            %% you need -include_lib("parse_trans/include/codegen.hrl"), and this
            %% will be added and exported as-is
            
            %% NB: you need a *little* understanding of erl_syntax to use these
            %% kinds of callbacks
            {FN, _} = erl_syntax:analyze_function(AST),
            [ {D, A#annotation{data=[{function, FN}|Data]}} || D <- Delegates ];
        _ ->
            %% TODO: clearer error handling API
            {error, "no delegates defined"}
    end.

codegen_advised(#annotation{data=AnnotationData}, M, F, Inputs) ->
    Argv = case lists:keyfind(prefix, 1, AnnotationData) of
        true ->
            [F|Inputs];
        false ->
            Inputs
    end,
    Target = lists:keyfind(target, 1, AnnotationData),
    erlang:apply(M, Target, [Argv]).
