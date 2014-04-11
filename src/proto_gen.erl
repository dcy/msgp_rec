-module(proto_gen).

-define(SRC_PROTOCOL_DIR, "../include/").
-define(SRC_PROTOCOL_FILE, "protocol_record.hrl").
-define(DST_MOD_DIR, "../src/").
-define(DST_MOD, "protocol").
%-define(DST_MOD_CLIENT, "mh_protocol").

-export([
         pack_list/3,
         unpack_list/4,
         gen/0
        ]).


gen() ->
    io:format("start gen...~n"),
    ProtocolFile = lists:concat([?SRC_PROTOCOL_DIR, ?SRC_PROTOCOL_FILE]),
    {ok, SourceTree} = epp:parse_file(ProtocolFile, ["./"], []),
    Fun = fun(Record) ->
                  case Record of
                      {attribute, _, type, {{record, _}, _, _}} -> true;
                      _ -> false
                  end
          end,
    RecordTree = lists:filter(Fun, SourceTree),
    gen_server_proto_erl(RecordTree),
	%gen_client_proto_h(RecordTree),
	%gen_client_proto_cpp(RecordTree),
    erlang:halt(),
    ok.

gen_server_proto_erl(RecordTree) ->
    DstModFile = lists:concat([?DST_MOD_DIR, ?DST_MOD, ".erl"]),
    case file:open(DstModFile, write) of
        {ok, _} ->
            gen_head(DstModFile),
            gen_packs(DstModFile, RecordTree),
            gen_unpacks(DstModFile, RecordTree),
            io:format("generate server proto erl finish~n");
        {error, enoent} ->
            io:format("~s does not exist~n", [DstModFile])
    end.

%gen_client_proto_h(RecordTypeL) ->
%	Dst_module_file = lists:concat([?DST_MOD_DIR, ?DST_MOD_CLIENT, ".h"]),
%	case file:open(Dst_module_file, write) of
%		{ok, _S} ->
%			gen_head_h(Dst_module_file),
%			gen_body_h(Dst_module_file, RecordTypeL),
%			gen_tail_h(Dst_module_file),
%			io:format("generate client proto h finish~n");
%		{error, enoent} ->
%			io:format("~s does not exist~n", [Dst_module_file])
%	end.
%
%gen_client_proto_cpp(RecordTypeL) ->
%	Dst_module_file = lists:concat([?DST_MOD_DIR, ?DST_MOD_CLIENT, ".cpp"]),
%	case file:open(Dst_module_file, write) of
%		{ok, _S} ->
%			gen_head_cpp(Dst_module_file),
%			gen_body_cpp(Dst_module_file, RecordTypeL),
%			io:format("generate client proto cpp finish~n");
%		{error, enoent} ->
%			io:format("~s does not exist~n", [Dst_module_file])
%	end.

gen_head(File) ->
    ModuleStr = io_lib:format("-module(~s).~n", [?DST_MOD]),
    file:write_file(File, ModuleStr, [append]),
    IncludeStr = io_lib:format("-include(\"~s\").~n", [?SRC_PROTOCOL_FILE]),
    file:write_file(File, IncludeStr, [append]),
    file:write_file(File, "-export([pack/2, unpack/2]).\n\n", [append]),

    ok.

gen_packs(File, RecordTree) ->
    [gen_pack(File, Record) || Record <- RecordTree],
    gen_pack_end(File).

gen_pack_end(File) ->
    write(File, "pack(Other, Rec) ->\n    erlang:error({Other, Rec}).\n\n").

gen_unpack_end(File) ->
    write(File, "unpack(Other, Bin) ->\n    erlang:error({Other, Bin}).\n\n").

gen_pack(File, {attribute, _, type, {{record, RecName}, RecFields, []}}) ->
    FunName = io_lib:format("pack(~s, Rec) ->\n", [RecName]),
    file:write_file(File, FunName, [append]),
    Fields = [pack_record_field(File, RecName, RecField) || RecField <- RecFields],
    FieldsStr = string:join(Fields, ", "),
    file:write_file(File, lists:concat(["    <<", FieldsStr, ">>;\n\n"]), [append]),
    ok.

pack_record_field(File, RecName, {typed_record_field, RF, TypeInfo}) ->
    {record_field, _, {atom, _, FieldName}} = RF,
    {type, _, union, [_, {type, _, Type, Info}]} = TypeInfo,
    case Type of
        record ->
            [{atom, _, FieldRecName}] = Info,
            FieldStr = io_lib:format("    B~s = pack(~s, Rec#~s.~s),\n", [FieldName, FieldRecName, RecName, FieldName]),
            file:write_file(File, FieldStr, [append]);
        list ->
            ItemType = get_item_type_by_info(Info),
            LengthStr = io_lib:format("    B~pLen = msgpack:pack(length(Rec#~s.~s)),\n", [FieldName, RecName, FieldName]),
            file:write_file(File, LengthStr, [append]),
            FieldStr = io_lib:format("    B~p = proto_gen:pack_list(Rec#~s.~s, B~sLen, ~p),\n", [FieldName, RecName, FieldName, FieldName, ItemType]),
            file:write_file(File, FieldStr, [append]);
        string ->
            FieldStr = io_lib:format("    B~p = msgpack:pack(tool:to_binary(Rec#~s.~s)),\n", [FieldName, RecName, FieldName]),
            file:write_file(File, FieldStr, [append]);
        _Other ->
            FieldStr = io_lib:format("    B~s = msgpack:pack(Rec#~s.~s),~n", [FieldName, RecName, FieldName]),
            file:write_file(File, FieldStr, [append])
    end,
    lists:concat(["B", FieldName, "/binary"]).

pack_list([], Bin, _Type) ->
    Bin;
pack_list([Field | Fields], Bin, Type) ->
    FieldBin = case Type of
                   string -> msgpack:pack(list_to_binary(Field));
                   {record, RecName} -> protocol:pack(RecName, Field);
                   _Other -> msgpack:pack(Field) 
               end,
    pack_list(Fields, <<Bin/binary, FieldBin/binary>>, Type).

unpack_list(0, Bin, List, _Type) ->
    {lists:reverse(List), Bin};
unpack_list(Len, Bin, List, Type) ->
    {Term, Rest} = case Type of
                       string ->
                           {TermString, StrRest} = msgpack:unpack_stream(Bin),
                           {erlang:binary_to_list(TermString), StrRest};
                       {record, RecName} ->
                           protocol:unpack(RecName, Bin);
                       _Other ->
                           msgpack:unpack_stream(Bin)
                   end,
    unpack_list(Len-1, Rest, [Term | List], Type).



gen_unpacks(File, RecordTree) ->
    [gen_unpack(File, Record) || Record <- RecordTree],
    gen_unpack_end(File),
    ok.

gen_unpack(File, {attribute, _, type, {{record, RecName}, RecFields, []}}) ->
    FunName = io_lib:format("unpack(~s, Bin) ->\n", [RecName]),
    write(File, FunName),
    Fields = [unpack_record_field(File, RecField) || RecField <- RecFields],
    FieldStr = string:join(Fields, ", "),
    write(File, lists:concat(["    {{", RecName, " ,", FieldStr, "}, ", get(last_unpack_bin_name), "};\n\n"])),
    erase(last_unpack_bin_name),
    ok.

unpack_record_field(File, {typed_record_field, RF, TypeInfo}) ->
    {record_field, _, {atom, _, FieldName}} = RF,
    {type, _, union, [_, {type, _, Type, Info}]} = TypeInfo,
    BinName = case get(last_unpack_bin_name) of
                  undefined -> "Bin";
                  LastBinName -> LastBinName
              end,
    FieldStr = case Type of
                   record ->
                       [{atom, _, FieldRecName}] = Info,
                       io_lib:format("    {Field~s, Bin~s} = unpack(~s, ~s),\n", [FieldName, FieldName, FieldRecName, BinName]);
                   list ->
                       ItemType = get_item_type_by_info(Info),
                       LenStr = io_lib:format("    {Len~s, BinLen~s} = msgpack:unpack_stream(~s),\n", [FieldName, FieldName, BinName]),
                       write(File, LenStr),
                       io_lib:format("    {Field~s, Bin~s} = proto_gen:unpack_list(Len~s, BinLen~s, [], ~p),\n", [FieldName, FieldName, FieldName, FieldName, ItemType]);
                   string ->
                       TempStr = io_lib:format("    {Field~sBin, Bin~s} = msgpack:unpack_stream(~s),\n", [FieldName, FieldName, BinName]),
                       write(File, TempStr),
                       io_lib:format("    Field~s = tool:to_list(Field~sBin),\n", [FieldName, FieldName]);
                   _Other ->
                       io_lib:format("    {Field~s, Bin~s} = msgpack:unpack_stream(~s),\n", [FieldName, FieldName, BinName])
               end,
    write(File, FieldStr),
    put(last_unpack_bin_name, lists:concat(["Bin", FieldName])),
    lists:concat(["Field", FieldName]).

write(File, Sth) ->
    file:write_file(File, Sth, [append]).

get_item_type_by_info(Info) ->
    case Info of
        [{type, _, string, []}] -> string;
        [{type, _, integer, []}] -> integer;
        [{type, _, integer64, []}] -> integer;
        [{type, _, float, []}] -> float;
        [{type, _, record, [{atom, _, ItemRecName}]}] -> {record, ItemRecName}
    end.


%%% -------------------- CLIENT H -----------------------------------------
%
%gen_head_h(File) ->
%	file:write_file(File, "/**\n"),
%	file:write_file(File, " * !!! DO NOT EDIT !!!\n", [append]),
%	file:write_file(File, " * GENERATED FROM PROTOCOL DEFINE FILES\n", [append]),
%	file:write_file(File, " */\n\n", [append]),
%	file:write_file(File, "#ifndef MH_PROTOCOL_H__\n", [append]),
%	file:write_file(File, "#define MH_PROTOCOL_H__\n\n", [append]),
%	file:write_file(File, "#include <vector>\n#include <string>\n#include <msgpack.hpp>\n\n#include \"mh_protocol_const.h\"\n\n", [append]).
%
%gen_body_h(File, RecordTypeL) ->
%	% empty struct
%	gen_body_empty_struct_h(File),
%	
%	[begin gen_body_struct_h(File, Data) end|| Data <- RecordTypeL].
%
%gen_body_empty_struct_h(File) ->
%	file:write_file(File, "struct ProtoEmpty {\n", [append]),
%	file:write_file(File, "\n	void pack(msgpack::packer<msgpack::sbuffer> *pac) const;", [append]),
%	file:write_file(File, "\n	void unpack(msgpack::unpacker *unpac);", [append]),
%	file:write_file(File, "\n	void unpack(const void *buf, int len);", [append]),
%	file:write_file(File, "\n};\n\n", [append]).
%
%gen_body_struct_h(File, {attribute, _, type, {{record, Record}, TypeRecordFieldL, []}}) ->
%	Str = underline_to_camel(erlang:atom_to_list(Record)),
%	Bytes = list_to_binary(io_lib:format("struct ~s {\n", [Str])),
%	file:write_file(File, Bytes, [append]),
%	
%	[begin gen_body_struct_h2(File, Data) end|| Data <- TypeRecordFieldL],
%	
%	file:write_file(File, "\n	void pack(msgpack::packer<msgpack::sbuffer> *pac) const;", [append]),
%	file:write_file(File, "\n	void unpack(msgpack::unpacker *unpac);", [append]),
%	file:write_file(File, "\n	void unpack(const void *buf, int len);", [append]),
%	
%	file:write_file(File, "\n};\n\n", [append]).
%
%gen_body_struct_h2(File, {typed_record_field, RF, TypeInfo}) ->
%	{record_field, _, {atom, _, FieldName}} = RF,
%	{type, _, union, [_, {type, _, Type, Info}]} = TypeInfo,
%	case Type of
%		record ->
%			[{atom, _, Record2}] = Info,
%			Str = "	" ++ to_client_type(Record2) ++ erlang:atom_to_list(FieldName) ++ ";\n",
%			file:write_file(File, Str, [append]);
%		list ->
%			Type2 = case Info of
%							[{type, _, string, []}] ->
%								string;
%							[{type, _, integer, []}] ->
%								integer;
%							[{type, _, integer64, []}] ->
%								integer64;
%							[{type, _, float, []}] ->
%								float;
%							[{type, _, record, [{atom, _, Record2}]}] ->
%								Record2
%						end,
%			Str = "	" ++ to_client_type({list, Type2}) ++ erlang:atom_to_list(FieldName) ++ ";\n",
%			file:write_file(File, Str, [append]);
%		_ ->
%			Str = "	" ++ to_client_type(Type) ++ erlang:atom_to_list(FieldName) ++ ";\n",
%			file:write_file(File, Str, [append])
%	end.
%
%gen_tail_h(File) ->
%	file:write_file(File, "#endif\n", [append]).
%
%%% ------------------------------------ CLIENT CPP ------------------------------------
%
%gen_head_cpp(File) ->
%	file:write_file(File, "/**\n"),
%	file:write_file(File, " * !!! DO NOT EDIT !!!\n", [append]),
%	file:write_file(File, " * GENERATED FROM PROTOCOL DEFINE FILES\n", [append]),
%	file:write_file(File, " */\n\n", [append]),
%	file:write_file(File, "#include \"mh_protocol.h\"\n#include \"pack_unpack_internal__.h\"\n\n", [append]).
%
%gen_body_cpp(File, RecordTypeL) ->
%	% empty struct
%	gen_body_empty_struct_cpp(File),
%	
%	[begin gen_body_struct_cpp(File, Data) end|| Data <- RecordTypeL].
%
%gen_body_empty_struct_cpp(File) ->
%	file:write_file(File, "void ProtoEmpty::pack(msgpack::packer<msgpack::sbuffer> *pack) const {\n", [append]),
%	file:write_file(File, "}\n\n", [append]),
%	
%	file:write_file(File, "void ProtoEmpty::unpack(msgpack::unpacker *unpac) {\n", [append]),
%	file:write_file(File, "}\n\n", [append]),
%	
%	file:write_file(File, "void ProtoEmpty::unpack(const void *buf, int len) {\n", [append]),
%	file:write_file(File, "	msgpack::unpacker upker;\n", [append]),
%	file:write_file(File, "	upker.reserve_buffer(len);\n", [append]),
%	file:write_file(File, "	memcpy(upker.buffer(), buf, len);\n", [append]),
%	file:write_file(File, "	upker.buffer_consumed(len);\n", [append]),
%	file:write_file(File, "	unpack(&upker);\n", [append]),
%	file:write_file(File, "}\n\n", [append]).
%
%gen_body_struct_cpp(File, {attribute, _, type, {{record, Record}, TypeRecordFieldL, []}}) ->
%	StrRecord = underline_to_camel(erlang:atom_to_list(Record)),
%	
%	Bytes = list_to_binary(io_lib:format("void ~s::pack(msgpack::packer<msgpack::sbuffer> *pack) const {\n", [StrRecord])),
%	file:write_file(File, Bytes, [append]),
%	[begin gen_body_struct_pack(File, Data) end|| Data <- TypeRecordFieldL],
%	file:write_file(File, "}\n\n", [append]),
%	
%	Bytes2 = list_to_binary(io_lib:format("void ~s::unpack(msgpack::unpacker *unpac) {\n", [StrRecord])),
%	file:write_file(File, Bytes2, [append]),
%	[begin gen_body_struct_unpack(File, Data) end|| Data <- TypeRecordFieldL],
%	file:write_file(File, "}\n\n", [append]),
%	
%	Bytes3 = list_to_binary(io_lib:format("void ~s::unpack(const void *buf, int len) {\n", [StrRecord])),
%	file:write_file(File, Bytes3, [append]),
%	file:write_file(File, "	msgpack::unpacker upker;\n", [append]),
%	file:write_file(File, "	upker.reserve_buffer(len);\n", [append]),
%	file:write_file(File, "	memcpy(upker.buffer(), buf, len);\n", [append]),
%	file:write_file(File, "	upker.buffer_consumed(len);\n", [append]),
%	file:write_file(File, "	unpack(&upker);\n", [append]),
%	file:write_file(File, "}\n\n", [append]).
%
%gen_body_struct_pack(File, {typed_record_field, RF, _TypeInfo}) ->
%	{record_field, _, {atom, _, FieldName}} = RF,
%	Bytes = list_to_binary(io_lib:format("	pack__(pack, ~p);\n", [FieldName])),
%	file:write_file(File, Bytes, [append]).
%
%gen_body_struct_unpack(File, {typed_record_field, RF, _TypeInfo}) ->
%	{record_field, _, {atom, _, FieldName}} = RF,
%	Bytes = list_to_binary(io_lib:format("	unpack__(unpac, &~p);\n", [FieldName])),
%	file:write_file(File, Bytes, [append]).
%	
%%% -------------------------------------- 工具函数 --------------------------------------
%
%%% 下划线转换为驼峰
%underline_to_camel(Str) ->
%	L = string:tokens(Str, "_"),
%	L2 = [begin initial_upper(Str2) end || Str2 <- L],
%	string:join(L2, "").
%
%%% 首字母大写
%initial_upper(Str) ->
%	[H|T] = Str,
%	H2 = string:to_upper(H),
%	[H2|T].
%
%% 将erlang类型转成c++类型（后面带空格）
%to_client_type(integer) ->
%	"int ";
%to_client_type(integer64) ->
%	"int64_t ";
%to_client_type(float) ->
%	"float ";
%to_client_type(string) ->
%	"std::string ";
%to_client_type({list, Type}) ->
%	"std::vector<" ++ to_client_type(Type) ++ "> ";
%to_client_type(RecordName) ->
%	Str = erlang:atom_to_list(RecordName),
%	underline_to_camel(Str) ++ " ".
