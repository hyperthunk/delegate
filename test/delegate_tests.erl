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
-module(delegate_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("hamcrest/include/hamcrest.hrl").
-include_lib("annotations/include/types.hrl").

-export([f1/1, f2/2, f3/3, f4/4]).

arg_unpacking_test_() ->
    [{"Prefixing with the called function",
      ?_assertThat(delegate(f4, ['$F', '$I'], [1, 2, 3]),
                 resolves_to([f4, 1, 2, 3]))},
     {"Suffixing with the called function",
      ?_assertThat(delegate(f3, ['$I', '$F'], [1, 2]),
                   resolves_to([1, 2, f3]))},
     {"Passing Inputs as a single positional argument",
      ?_assertThat(delegate(f1, [['$I']], [1, 2, 3]),
                     resolves_to([[1, 2, 3]]))},
     {"Prefixing with MFA",
      ?_assertThat(delegate(f4, ['$M', '$F', '$A', '$I'], [1]),
                       resolves_to([?MODULE, f4, 1, 1]))},
     {"Nested Data Structures",
      ?_assertThat(delegate(f3, [{'$M', '$F', '$A'}, '$I'], [1, 2]),
                         resolves_to([{?MODULE, f3, 2}, 1, 2]))},
     {"Appending Nested Data Structures",
       ?_assertThat(delegate(f2, [['$I'], {'$M', '$F', '$A'}], [1, 2]),
                          resolves_to([[[1, 2]], {?MODULE, f2, 2}]))}].

arg_tuple_handling_test_() ->
    [{"Basic Tuple Unpacking",
     ?_assertThat(delegate(f1, {'$F', '$I'}, [1, 2, 3]),
                     resolves_to({'f1', [1, 2, 3]}))},
     {"Nested Tuple Data",
     ?_assertThat(delegate(f1, {call, {'$M', '$T', ['$I']}}, [1, 2, 3]),
                      resolves_to({call, {?MODULE, 'f1', [1, 2, 3]}}))}].

resolves_to(Inputs) ->
    equal_to(Inputs).

delegate(Func, InputSpec, Inputs) ->
    A=#annotation{ name=?MODULE,
                   data=[{target, Func},
                         {args, InputSpec}]},
    catch(delegate:delegate_advice(A, ?MODULE, Func, Inputs)).

f1(V1) ->
    V1.

f2(V1, V2) ->
    [V1, V2].

f3(V1, V2, V3) ->
    [V1, V2, V3].

f4(V1, V2, V3, V4) ->
    [V1, V2, V3, V4].
