-module(accumulator).

-export([new/1,new/2,add/2,sum/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {
	data :: list(integer()),
	groups :: integer(),
	group_fun :: fun(() -> non_neg_integer())
}).

-spec new(non_neg_integer()) -> #state{}.
new(Groups) ->
	new(Groups, fun epoch/0).

-spec new(non_neg_integer(), fun(() -> non_neg_integer())) -> #state{}.
new(Groups,GroupFun) ->
	#state{data=[],groups=Groups,group_fun=GroupFun}.

add(State=#state{group_fun=GroupFun}, Value) when is_integer(Value) ->
	add_core(State, Value, GroupFun()).

add_core(State=#state{data=[]}, Value, Group) ->
	State#state{data=[{Group,Value}]};
add_core(State=#state{data=[{Group,Acc}|Tail]}, Value, Group) ->
	State#state{data=[{Group,(Acc+Value)}|Tail]};
add_core(State=#state{data=Data}, Value, Group) ->
	State#state{data=[{Group,Value}|Data]}.

-spec sum(#state{}) -> {#state{}, non_neg_integer()}.
sum(State=#state{data=[]}) ->
	{State,0};
sum(State=#state{data=Data,groups=Groups,group_fun=GroupFun}) ->
	CurrentGroup = GroupFun(),
	CutOff = CurrentGroup - Groups,
	NewData = [{G,V} || {G,V} <- Data, G >= CutOff],
	SumGroups = [V || {G,V} <- NewData, G =/= CurrentGroup],
	Sum = lists:sum(SumGroups),
	{State#state{data=NewData}, Sum}.

-spec epoch() -> non_neg_integer().
epoch() ->
	{MegaSecs,Secs,_MicroSecs} = erlang:now(),
	(MegaSecs*1000000 + Secs).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

empty_test() ->
	Acc1 = accumulator:new(1),
	{_Acc2, Sum1} = accumulator:sum(Acc1),
	?assertEqual(0, Sum1).

one_test() ->
	Acc1 = accumulator:new(1, fun() -> 0 end),
	Acc2 = accumulator:add(Acc1, 7),
	{_, Sum1} = accumulator:sum(Acc2#state{group_fun=fun() -> 1 end}),
	?assertEqual(7, Sum1).

two_test() ->
	Acc1 = accumulator:new(1, fun() -> 0 end),
	Acc2 = accumulator:add(Acc1, 7),
	Acc3 = accumulator:add(Acc2, 8),
	{_, Sum1} = accumulator:sum(Acc3#state{group_fun=fun() -> 1 end}),
	?assertEqual(15, Sum1).

activebucket_test() ->
	Acc1 = accumulator:new(1, fun() -> 0 end),
	Acc2 = accumulator:add(Acc1, 7),
	Acc3 = accumulator:add(Acc2, 8),
	Acc4 = accumulator:add(Acc3#state{group_fun=fun() -> 1 end}, 9),
	{_, Sum1} = accumulator:sum(Acc4),
	?assertEqual(15, Sum1).

splice_test() ->
	Acc1 = accumulator:new(1, fun() -> 0 end),
	Acc2 = accumulator:add(Acc1, 7),
	Acc3 = accumulator:add(Acc2#state{group_fun=fun() -> 1 end}, 8),
	Acc4 = accumulator:add(Acc3#state{group_fun=fun() -> 2 end}, 9),
	{_, Sum1} = accumulator:sum(Acc4),
	?assertEqual(8, Sum1).

flatline_bug_test() ->
	Acc1 = accumulator:new(10, fun() -> 0 end),
	Acc2 = accumulator:add(Acc1, 1),
	Acc3 = accumulator:add(Acc2#state{group_fun=fun() -> 1 end}, 1),
	Acc4 = accumulator:add(Acc3#state{group_fun=fun() -> 2 end}, 1),
	Acc5 = accumulator:add(Acc4#state{group_fun=fun() -> 3 end}, 1),
	Acc6 = accumulator:add(Acc5#state{group_fun=fun() -> 4 end}, 1),
	Acc7 = accumulator:add(Acc6#state{group_fun=fun() -> 5 end}, 1),
	Acc8 = accumulator:add(Acc7#state{group_fun=fun() -> 6 end}, 1),
	Acc9 = accumulator:add(Acc8#state{group_fun=fun() -> 7 end}, 1),
	Acc10 = accumulator:add(Acc9#state{group_fun=fun() -> 8 end}, 1),
	Acc11 = accumulator:add(Acc10#state{group_fun=fun() -> 9 end}, 1),
	Acc12 = accumulator:add(Acc11#state{group_fun=fun() -> 10 end}, 1),
	Acc13 = accumulator:add(Acc12#state{group_fun=fun() -> 11 end}, 1),
	
	%% now we have '1' in 12 buckets.
	{Acc14, Sum1} = accumulator:sum(Acc13),
	?assertEqual(10, Sum1),

	{_Acc15, Sum2} = accumulator:sum(Acc14#state{group_fun=fun() -> 17 end}),
	?assertEqual(5, Sum2).

-endif.
