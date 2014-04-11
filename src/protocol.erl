-module(protocol).
-include("protocol_record.hrl").
-export([pack/2, unpack/2]).

pack(position_info, Rec) ->
    Bposition = msgpack:pack(Rec#position_info.position),
    Bgeneral_id = msgpack:pack(Rec#position_info.general_id),
    <<Bposition/binary, Bgeneral_id/binary>>;

pack(formation_info, Rec) ->
    Bformation_id = msgpack:pack(Rec#formation_info.formation_id),
    BpositionsLen = msgpack:pack(length(Rec#formation_info.positions)),
    Bpositions = proto_gen:pack_list(Rec#formation_info.positions, BpositionsLen, {record,
                                                                                   position_info}),
    <<Bformation_id/binary, Bpositions/binary>>;

pack(set_formation_req, Rec) ->
    Bformation_info = pack(formation_info, Rec#set_formation_req.formation_info),
    <<Bformation_info/binary>>;

pack(set_formation_resp, Rec) ->
    Bret_code = msgpack:pack(Rec#set_formation_resp.ret_code),
    <<Bret_code/binary>>;

pack(Other, Rec) ->
    erlang:error({Other, Rec}).

unpack(position_info, Bin) ->
    {Fieldposition, Binposition} = msgpack:unpack_stream(Bin),
    {Fieldgeneral_id, Bingeneral_id} = msgpack:unpack_stream(Binposition),
    {{position_info ,Fieldposition, Fieldgeneral_id}, Bingeneral_id};

unpack(formation_info, Bin) ->
    {Fieldformation_id, Binformation_id} = msgpack:unpack_stream(Bin),
    {Lenpositions, BinLenpositions} = msgpack:unpack_stream(Binformation_id),
    {Fieldpositions, Binpositions} = proto_gen:unpack_list(Lenpositions, BinLenpositions, [], {record,
                                                                                               position_info}),
    {{formation_info ,Fieldformation_id, Fieldpositions}, Binpositions};

unpack(set_formation_req, Bin) ->
    {Fieldformation_info, Binformation_info} = unpack(formation_info, Bin),
    {{set_formation_req ,Fieldformation_info}, Binformation_info};

unpack(set_formation_resp, Bin) ->
    {Fieldret_code, Binret_code} = msgpack:unpack_stream(Bin),
    {{set_formation_resp ,Fieldret_code}, Binret_code};

unpack(Other, Bin) ->
    erlang:error({Other, Bin}).

