%% @author weichengjun
%% @doc @todo Add description to packet.
-module(packet).

-ifndef(PACKET_HRL).
-define(PACKET_HRL, true).

-define(PACK_HEAD_LENGTH, 9).
-define(UNPACK_HEAD_LENGTH, 7).
-define(NO_ERR_ID, 0).
-define(EMPTY_DATA_SIZE, 0).

%% DataStatus
-define(RAW_DATA, 0).
-define(COMPRESSION_DATA, 1).
-define(ENCRYPTION_DATA, 2).

-endif.
%% ====================================================================
%% API functions
%% ====================================================================
-export([unpack/3, unpack/4, pack/4, pack_error/3, pack_client/2]).
-export([get_data_status/1]).
-export([packet/1
        ,unpacket/2
        ,pack/3
    ]).

get_data_status(Atom) ->
	case Atom of
		raw ->
			?RAW_DATA;
		compression ->
			?COMPRESSION_DATA;
		encryption ->
			?ENCRYPTION_DATA
	end.


pack(Cmd, Data, Flag) ->
	case Data of
        null ->
           	 pack_empty(Cmd, Flag);
        _ ->
            pack_proto(Cmd, 0, Data, Flag)
    end.

pack(Cmd, DateStatus, Data, Flag) ->
	case Data of
        null ->
            pack_empty(Cmd, Flag);
        _ ->
            pack_proto(Cmd, DateStatus, Data, Flag)
    end.

pack_empty(Cmd, Flag) ->
%%    BB = <<0:16/little, 0:16/little, 0:8, Flag:32/little, Cmd:16/little>>,
	BB = <<0:16, 0:16, 0:8, Flag:32, Cmd:16>>,
%%	WebBin = web_packet:pack_web_bin(BB),

    {ok, BB}.

pack_proto(Cmd, DateStatus, Data, Flag) ->
	String = "encode_m_" ++ erlang:integer_to_list(Cmd) ++ "_toc",
	Fun = list_to_atom1(String),
	PbList = all_pb:Fun(Data),
	Bin = erlang:iolist_to_binary(PbList),
	NewBin = handl_data_out(DateStatus, Bin),
	NewBin = Bin,
	ErrorID = ?NO_ERR_ID,
	DataSize = erlang:byte_size(NewBin),
	BB = <<ErrorID:16, DataSize:16, DateStatus:8, Flag:32, Cmd:16, NewBin/binary>>,
	{ok, BB}.

unpack(Cmd, DataStatus, Bin) ->
	NewBin = handl_data_in(DataStatus, Bin),
	String = "decode_m_" ++ erlang:integer_to_list(Cmd) ++ "_tos",
	Fun = list_to_atom1(String),
	Data = all_pb:Fun(NewBin),
	{ok, Cmd, Data}.

unpack(Cmd, DataStatus, Bin, toc) ->
	NewBin = handl_data_in(DataStatus, Bin),
	String = "decode_m_" ++ erlang:integer_to_list(Cmd) ++ "_toc",
	Fun = list_to_atom1(String),
	Data = all_pb:Fun(NewBin),
	{ok, Cmd, Data}.

pack_error(Cmd, ErrorID, Flag) ->
	DataSize = ?EMPTY_DATA_SIZE,
	DataStatus = ?RAW_DATA,
	BB = <<ErrorID:16, DataSize:16, DataStatus:8, Flag:32, Cmd:16>>,
	{ok, BB}.

pack_client(Cmd, Data) ->
	String = "encode_m_" ++ erlang:integer_to_list(Cmd) ++ "_tos",
	Fun = list_to_atom1(String),
	PbList = all_pb:Fun(Data),
	Bin = erlang:iolist_to_binary(PbList),
	DataSize = erlang:byte_size(Bin),
	BB = <<DataSize:16, 0:8, Cmd:16, Bin/binary>>,
	{ok, BB}.

%% ====================================================================
%% Internal functions
%% ====================================================================


handl_data_out(DataStatus, Data) ->
	case DataStatus of
		?RAW_DATA ->
			Data;
		?COMPRESSION_DATA->
			compress_data(Data);
		?ENCRYPTION_DATA ->
			encrypt_data(Data)
	end.

%% 压缩数据
compress_data(Data) ->
	zlib:compress(Data).

%% 加密数据
encrypt_data(Data) ->
	%% TODO 未实现
	Data.

%% ====================================================================

handl_data_in(DataStatus, Data) ->
	case DataStatus of
		?RAW_DATA ->
			Data;
		?COMPRESSION_DATA->
			uncompress_data(Data);
		?ENCRYPTION_DATA ->
			decrypt_data(Data)
	end.

%% 解压数据
uncompress_data(Data) ->
	zlib:uncompress(Data).

%% 解密密数据
decrypt_data(Data) ->
	%% TODO 未实现
	Data.

%%打包数据
packet(Bin) ->
	DataSize = erlang:byte_size(Bin),
	{Len, ExLen} =
		case DataSize of
			D when D < 126 ->
				{DataSize, 0};
			D when D < 65536 ->
				{126, D};
			_ ->
				{127, DataSize}
		end,
	Frame =
		case ExLen of
			0 ->
				<<1:1, 0:3, 2:4, 0:1, Len:7, Bin/binary>>;
			E when E > 65535 ->
				<<1:1, 0:3, 2:4, 0:1, Len:7, ExLen:64, Bin/binary>>;
			_ ->
				<<1:1, 0:3, 2:4, 0:1, Len:7, ExLen:16, Bin/binary>>
		end,
	Frame.


%%  解析数据
unpacket(Bin, Len) ->
    <<Masking:4/binary, Payload:Len/binary>> = Bin,
	unpacket(Payload, Masking, <<>>).

unpacket(Payload, <<MA:8, MB:8, MC:8, MD:8>> = Masking, Acc) ->
	case size(Payload) of
		0 ->
			Acc;
		1 ->
			<<A:8>> = Payload,
			<<Acc/binary, (MA bxor A)>>;
		2 ->
			<<A:8, B:8>> = Payload,
			<<Acc/binary, (MA bxor A), (MB bxor B)>>;
		3 ->
			<<A:8, B:8, C:8>> = Payload,
			<<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C)>>;
		_Other ->
			<<A:8, B:8, C:8, D:8, Rest/binary>> = Payload,
			Acc1 = <<Acc/binary, (MA bxor A), (MB bxor B), (MC bxor C), (MD bxor D)>>,
			unpacket(Rest, Masking, Acc1)
	end.

list_to_atom1(List) when is_list(List) ->
    case catch(list_to_existing_atom(List)) of
        {'EXIT', _} ->
            erlang:list_to_atom(List);
        Atom ->
            Atom
    end.

