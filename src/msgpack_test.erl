-module(msgpack_test).
-export([start/1]).
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
    PositionInfo1 = #position_info{position = 1,
                                   general_id = 2},
    PositionInfo2 = #position_info{position = 2,
                                   general_id = 12},
    PositionInfo3 = #position_info{position = 3,
                                   general_id = 22},
    FormationInfo = #formation_info{formation_id = 1,
                                    positions = [PositionInfo1, PositionInfo2, PositionInfo3]},
    Req = #set_formation_req{formation_info = FormationInfo},
    {ok, FormationInfo} =:= protocol:unpack(set_formation_req, protocol:pack(set_formation_req, Req)).



get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro)/1000.
