-ifndef(__PROTOCOL_RECORD_H__).
-define(__PROTOCOL_RECORD_H__, 0).

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


-endif.
