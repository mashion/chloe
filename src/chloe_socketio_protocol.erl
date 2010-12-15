-module(chloe_socketio_protocol).

%% API
-export([
         parse/1
        ]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

parse(Data) ->
    {ok, Type, Body} = parse_type(binary_to_list(Data)),
    {ok, Length, Body2} = parse_length(Body),
    {ok, PayLoad} = parse_payload(Length, Body2),
    {ok, Type, PayLoad}.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------
parse_type([TypeId | [$: | Data]]) ->
    Type = case TypeId of
               $1 -> message
           end,
    {ok, Type, Data}.

parse_length(Data) ->
    parse_length(Data, "").

parse_length([LengthChar | [$: | Data]], LengthCars) ->
    Length = list_to_integer(LengthCars ++ [LengthChar]),
    {ok, Length, Data};
parse_length([LengthChar | Data], LengthCars) ->
    parse_length(Data, LengthCars ++ [LengthChar]).

% The Length + 1 is the length of the message, plus 1 for the ',' at 
% the end
parse_payload(Length, Data) when length(Data) =:= Length + 1 ->
    {ok, lists:sublist(Data, Length)}.

