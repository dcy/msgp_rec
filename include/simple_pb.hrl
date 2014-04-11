-ifndef(POSITIONINFO_PB_H).
-define(POSITIONINFO_PB_H, true).
-record(positioninfo, {
    position = erlang:error({required, position}),
    general_id = erlang:error({required, general_id})
}).
-endif.

-ifndef(FORMATIONINFO_PB_H).
-define(FORMATIONINFO_PB_H, true).
-record(formationinfo, {
    formation_id = erlang:error({required, formation_id}),
    positions = []
}).
-endif.

-ifndef(SETFORMATION_PB_H).
-define(SETFORMATION_PB_H, true).
-record(setformation, {
    formation_info = erlang:error({required, formation_info})
}).
-endif.

