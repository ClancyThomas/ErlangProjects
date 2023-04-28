-module(arithmetic).
-export([start_factorializer/0,start_adder/0,start_subtracter/0,start_multiplier/0,start_divider/0,
		 factorializer/0,adder/0,subtracter/0,multiplier/0,divider/0,
		 factorial_of/2,add/3,subtract/3,multiply/3,divide/3]).

%%
%% Spawn the processes for each operation
%%

start_factorializer()->
	spawn(?MODULE, factorializer).

start_adder() ->
	spawn(?MODULE, adder).

start_subtracter() ->
	spawn(?MODULE, subtracter).

start_multiplier() ->
	spawn(?MODULE, multiplier).

start_divider() ->
	spawn(?MODULE, divider).

%%--------------------------
%% Client functions
%%--------------------------

factorial_of(Pid,Num) ->
	Pid ! {self(), Num},
	receive
		Response ->
			Response
	end.

add(Pid,NumOne,NumTwo) ->
	Pid ! {self(), NumOne, NumTwo},
	receive
		Response ->
			Response
	end.

subtract(Pid,NumOne,NumTwo) ->
	Pid ! {self(), NumOne, NumTwo},
	receive
		Response ->
			Response
	end.

multiply(Pid,NumOne,NumTwo) ->
	Pid ! {self(), NumOne, NumTwo},
	receive
		Response ->
			Response
	end.

divide(Pid,NumOne,NumTwo) ->
	Pid ! {self(), NumOne, NumTwo},
	receive
		Response ->
			Response
	end.

%%---------------------------
%% Server functions
%%---------------------------

factorializer() -> 
	receive
		{Pid,Num} when is_integer(Num), Num == 0 ->
			Pid ! 1;
		{Pid,Num} when is_integer(Num), Num > 0 ->
			self() ! {Pid,Num-1,Num};
        {Pid,Num,Accumulator} when is_integer(Num), Num == 0 ->
			Pid ! Accumulator;
        {Pid,Num,Accumulator} when is_integer(Num), Num > 0 ->
			self() ! {Pid,Num-1,Accumulator*Num};
		{Pid,Num} when is_integer(Num), Num < 0 ->
			Pid ! {fail, Num, is_negative};
		{Pid,Num} ->
			Pid ! {fail, Num, is_not_integer}
	end,
	factorializer().

adder() ->
	receive
		{Pid,NumOne,NumTwo} when is_number(NumOne), is_number(NumTwo) ->
			Pid ! NumOne + NumTwo;
		{Pid,NumOne,_}when is_atom(NumOne) ->
			Pid ! {fail, NumOne, is_not_number};
		{Pid,_,NumTwo} when is_atom(NumTwo) ->
			Pid ! {fail, NumTwo, is_not_number};
		{Pid,_,_} ->
			Pid ! {fail, unrecognized_message}
	end,
	adder().

subtracter() ->
	receive
		{Pid,NumOne,NumTwo} when is_number(NumOne), is_number(NumTwo) ->
			Pid ! NumOne - NumTwo;
		{Pid,NumOne,_}when is_atom(NumOne) ->
			Pid ! {fail, NumOne, is_not_number};
		{Pid,_,NumTwo} when is_atom(NumTwo) ->
			Pid ! {fail, NumTwo, is_not_number};
		{Pid,_,_} ->
			Pid ! {fail, unrecognized_message}
	end,
	subtracter().

multiplier() ->
	receive
		{Pid,NumOne,NumTwo} when is_number(NumOne), is_number(NumTwo) ->
			Pid ! NumOne * NumTwo;
		{Pid,NumOne,_}when is_atom(NumOne) ->
			Pid ! {fail, NumOne, is_not_number};
		{Pid,_,NumTwo} when is_atom(NumTwo) ->
			Pid ! {fail, NumTwo, is_not_number};
		{Pid,_,_} ->
			Pid ! {fail, unrecognized_message}
	end,
	multiplier().

divider() ->
	receive
		{Pid,NumOne,NumTwo} when is_number(NumOne), is_number(NumTwo) ->
			Pid ! NumOne / NumTwo;
		{Pid,NumOne,_}when is_atom(NumOne) ->
			Pid ! {fail, NumOne, is_not_number};
		{Pid,_,NumTwo} when is_atom(NumTwo) ->
			Pid ! {fail, NumTwo, is_not_number};
		{Pid,_,_} ->
			Pid ! {fail, unrecognized_message}
	end,
	divider().


-ifdef(EUNIT).
%%
%% Unit tests go here. 
%%

-include_lib("eunit/include/eunit.hrl").


factorializer_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,factorializer,[]),	
			register(test_factorializer,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assertEqual(120,factorial_of(test_factorializer,5)),%happy path
	  %nasty thoughts start here
	  ?_assertEqual(1,factorial_of(test_factorializer,0)),
	  ?_assertEqual(479001600,factorial_of(test_factorializer,12)),
	  ?_assertEqual({fail,-3,is_negative},factorial_of(test_factorializer,-3)),
	  ?_assertEqual({fail,bob,is_not_integer},factorial_of(test_factorializer,bob)),
	  ?_assertEqual({fail,5.0,is_not_integer},factorial_of(test_factorializer,5.0))
	]
}.

adder_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,adder,[]),	
			register(test_adder,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assertEqual(8,add(test_adder,5,3)),%happy path
	  %nasty thoughts start here
	  ?_assertEqual(0,add(test_adder,0,0)),
	  ?_assertEqual(0.0,add(test_adder,0.0,0.0)),
	  ?_assertEqual(0,add(test_adder,-5,5)),
	  ?_assertEqual(1.5,add(test_adder,0.75,0.75)),
	  ?_assertEqual({fail,bob,is_not_number},add(test_adder,bob,3)),
	  ?_assertEqual({fail,sue,is_not_number},add(test_adder,3,sue)),
	  ?_assertEqual({fail,bob,is_not_number},add(test_adder,bob,sue))
	]
}.

subtracter_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,subtracter,[]),	
			register(test_subtracter,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assertEqual(2,subtract(test_subtracter,5,3)),%happy path
	  %nasty thoughts start here
	  ?_assertEqual(0,subtract(test_subtracter,0,0)),
	  ?_assertEqual(0.0,subtract(test_subtracter,0.0,0.0)),
	  ?_assertEqual(-10,subtract(test_subtracter,-5,5)),
	  ?_assertEqual(0.75,subtract(test_subtracter,1.5,0.75)),
	  ?_assertEqual({fail,bob,is_not_number},subtract(test_subtracter,bob,3)),
	  ?_assertEqual({fail,sue,is_not_number},subtract(test_subtracter,3,sue)),
	  ?_assertEqual({fail,bob,is_not_number},subtract(test_subtracter,bob,sue))
	]
}.

multiplier_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,multiplier,[]),	
			register(test_multiplier,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assertEqual(15,multiply(test_multiplier,5,3)),%happy path
	  %nasty thoughts start here
	  ?_assertEqual(0,multiply(test_multiplier,0,0)),
	  ?_assertEqual(0.0,multiply(test_multiplier,0.0,0.0)),
	  ?_assertEqual(-25,multiply(test_multiplier,-5,5)),
	  ?_assertEqual(1.125,multiply(test_multiplier,1.5,0.75)),
	  ?_assertEqual({fail,bob,is_not_number},multiply(test_multiplier,bob,3)),
	  ?_assertEqual({fail,sue,is_not_number},multiply(test_multiplier,3,sue)),
	  ?_assertEqual({fail,bob,is_not_number},multiply(test_multiplier,bob,sue))
	]
}.

divider_test_() ->
{setup,
	fun()->%runs before any of the tests
			Pid = spawn(?MODULE,divider,[]),	
			register(test_divider,Pid)
		end,
	%fun(_)->%runs after all of the tests
		%there is no teardown needed, so this fun doesn't need to be implemented.
	%end,
	%factorializer tests start here
	[ ?_assert((1.6 < divide(test_divider,5,3)) and (divide(test_divider,5,3) < 1.7)),%happy path
	  %nasty thoughts start here
	  ?_assertEqual(-1.0,divide(test_divider,-5,5)),
	  ?_assertEqual(2.0,divide(test_divider,1.5,0.75)),
	  ?_assertEqual({fail,bob,is_not_number},divide(test_divider,bob,3)),
	  ?_assertEqual({fail,sue,is_not_number},divide(test_divider,3,sue)),
	  ?_assertEqual({fail,bob,is_not_number},divide(test_divider,bob,sue))
	]
}.

-endif.