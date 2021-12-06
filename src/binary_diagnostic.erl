-module(binary_diagnostic).
-export([power/1, oxygen/1]).

power(Input) ->
    {Gamma, Epsilon} = power_bits(Input),
    {bits_to_integer(Gamma), bits_to_integer(Epsilon)}.

oxygen(Input) ->
    Oxygen = bit_criteria(Input, most),
    CO2 = bit_criteria(Input, least),
    {bits_to_integer(Oxygen), bits_to_integer(CO2)}.

bits_to_integer(Input) ->
    list_to_integer(lists:map(fun(X) -> X + $0 end, Input), 2).

power_bit(L) ->
    Fn = fun([V|_], Acc) -> V + Acc end,
    Sum = lists:foldl(Fn, 0, L),
    if Sum / length(L) < 0.5 -> 0;
       true -> 1
    end.

power_bits(Input) ->
    TInput=[H|_] = list_utils:transpose(Input),
    Length = length(H),
    Fn = fun(L) ->
                 S = lists:sum(L),
                 if S / Length < 0.5 -> {0, 1};
                    true -> {1, 0}
                 end
         end,
    lists:unzip(lists:map(Fn, TInput)).

bit_criteria(Input, Criteria) ->
    PBit = power_bit(Input),
    Bit = case {Criteria, PBit} of
              {most, B} -> B;
              {least, 0} -> 1;
              _ -> 0
          end,
    Matched = lists:filter(fun([X|_]) -> X =:= Bit end, Input),
    if length(Matched) =:= 1 -> hd(Matched);
       true ->
           Tails = lists:map(fun tl/1, Matched),
           [Bit|bit_criteria(Tails, Criteria)]
    end.
