msgp_rec
========

msgpack-erlang for record


[msgpack-erlang](https://github.com/msgpack/msgpack-erlang) can not pack erlang's record.
But our client and server should communicate according the protocol file.
The best describe way must be record, so we should pack and unpack record.

How can i use msgp_rec?
-----------------------

# First
Define the protocol, see "include/protocol_record.hrl"
`
-record(position_info, {position   :: integer(), % 位置
						general_id :: integer()  % 武将id
					   }).

-record(formation_info, {formation_id :: integer(),				% 阵法id
						 positions    :: list(#position_info{})	% 站位
						}).

%% ===========================阵法, 分类号:19=======================
%% 19001 设置阵法
-record(set_formation_req, {formation_info :: #formation_info{}	                    % 阵法信息
                           }).

-record(set_formation_resp, {ret_code :: integer() 
                            }).
`
