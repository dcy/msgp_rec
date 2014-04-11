-module(protobuff_test).
-export([start/1]).
-include("simple_pb.hrl").
-include("protocol_record.hrl").

start(Num) ->
    Start = get_timestamp(),
    protocol(Num),
    End = get_timestamp(),
    io:format("*****Cost MilSeconds: ~p~n", [End - Start]),
    ok.

protocol(0) ->
    ok;
protocol(Num) ->
    set_formation(),
    protocol(Num-1).

set_formation() ->
    PositionInfo1 = #positioninfo{position = 1, general_id = 2},
    PositionInfo2 = #positioninfo{position = 2, general_id = 12},
    PositionInfo3 = #positioninfo{position = 3, general_id = 22},
    FormationInfo = #formationinfo{formation_id = 1, positions = [PositionInfo1, PositionInfo2, PositionInfo3]},
    SetFormation = #setformation{formation_info = FormationInfo},
    EncodeBin = erlang:iolist_to_binary(simple_pb:encode_setformation(SetFormation)),
    SetFormation =:= simple_pb:decode_setformation(EncodeBin).

get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro)/1000.
