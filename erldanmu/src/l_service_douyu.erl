-module(l_service_douyu).

% -define(HOST, "openbarrage.douyutv.com").
-define(HOST, "danmuproxy.douyu.com").
-define(PROT, 8601).

-include("common.hrl").
-define(HEART_TIME, 40 * 1000). %% 40秒一个心跳
%% ====================================================================
%% API functions
%% ====================================================================
-export([open/1]).

-export ([login/2,
          analyze_data/1,
          heart/1,
          logout/1]).

open(Url) when is_list(Url) ->
    case get_room_id(Url) of
        error ->
            ?PRINT("error url ~p ~n", [Url]),
            error;
        RoomId ->
            open(RoomId)
    end;
open(RoomId) when is_integer(RoomId) ->
    m_danmu:start(?HOST, ?PROT, ?MODULE, RoomId);
open(Params) ->
    ?PRINT("Error Open params: ~p~n", [Params]).

login(Socket, RoomId) ->
    LoginData = lists:flatten(io_lib:format("type@=loginreq/roomid@=~p/", [RoomId])),
    case send(Socket, pack_data(LoginData)) of
        ok ->
            %% gid不知道哪一个比较合适，弹幕量多的情况下，服务器没有返回部分弹幕信息
            GroupData = lists:flatten(io_lib:format("type@=joingroup/rid@=~p/gid@=-9999/", [RoomId])),
            send(Socket, pack_data(GroupData));
        _ ->
            error
    end.

heart(Socket) ->
    erlang:send_after(?HEART_TIME, self(), {heart}),
    % 旧版心跳
    Now =  m_timer:now_seconds(),
    OldHeartData = lists:flatten(io_lib:format("type@=keeplive/tick@=~p/", [Now])),
    send(Socket, OldHeartData),
    % 新版心跳
    HeartData = "type@=mrkl/",
    send(Socket, HeartData).

logout(Socket) ->
    LogoutData = "type@=logout/",
    Bin = pack_data(LogoutData),
    send(Socket, Bin).

analyze_data(<<_Len:32/little, _:32, _MsgType:16/little, _:8, _:8, Bin/binary>> = DataBin) ->
    case Bin of
        <<Str/binary>> ->
            Msg = binary:split(Str, <<$/>>, [global, trim]),
            Fun = fun(I, L) ->
                    case binary:split(I, <<"@=">>, [global, trim]) of
                        [KeyBin, ValueBin] ->
                            Key = binary_to_atom(KeyBin, latin1),
                            case Key of
                                _ when Key == type ->
                                    Value = binary_to_atom(ValueBin, latin1);
                                _ when Key == hits ->
                                    Value = binary_to_integer(ValueBin);
                                _ ->
                                    Value = ValueBin
                            end,
                            [{Key, Value} | L];
                        _ ->
                            L
                    end
                end,
            MsgList = lists:foldl(Fun, [], Msg),
            print_msg(MsgList);
        _ ->
            ?PRINT("Skip: ~ts~n", [DataBin]),
            skip
    end;

analyze_data(_Data) ->
    % ?PRINT("error data: ~p~n", [Data]).
    skip.

%% ====================================================================
%% Internal functions
%% ====================================================================
pack_data(Data) ->
    MsgType = 689,  %% 消息类型:客户端发送给弹幕服务器的文本格式数据
    Encipher = 0,   %% 加密字段
    LessField = 0,  %% 保留字段
    EndField = "\0",
    Len = length(Data) + 9,
    <<Len:32/little, Len:32/little, MsgType:16/little, Encipher:8/little, LessField:8/little, 
            (list_to_binary(Data))/binary, 
            (list_to_binary(EndField))/binary>>.

send(Socket, Bin) ->
    case gen_tcp:send(Socket, Bin) of
        ok ->
            ok;
        _ ->
            ?PRINT("Error send : ~p~n", [Bin]),
            error
    end.

print_msg(Msg) ->
    case lists:keyfind(type, 1, Msg) of
        {_, chatmsg} ->
            {_, Name} = lists:keyfind(nn, 1, Msg),
            {_, Content} = lists:keyfind(txt, 1, Msg),
            ?PRINT("~ts : ~ts~n", [Name, Content]);
        % {_, loginres} ->
        %     ?PRINT("Msg: ~p~n", [Msg]);
        % 赠送需要依据礼物id获取对应的礼物名称
        % {_, dgb} ->
        %     {_, Name} = lists:keyfind(nn, 1, Msg),
        %     GifNum = get_msg_num(gfcnt, Msg),
        %     Hits = get_msg_num(hits, Msg),
        %     ?PRINT("~ts 赠送 ~p ~p 连击 ~n", [Name, GifNum, Hits]);
        _Type ->
            skip
    end.

% get_msg_num(Key, Msg) ->
%     case lists:keyfind(Key, 1, Msg) of
%         false ->
%             1;
%         {_, Num} ->
%             Num
%     end.

get_room_id(Url) ->
    case httpc:request(Url) of
        {ok, Data} ->
            {_, _, Content} = Data,
            case re:run(Content, "rel=\"canonical\".*?com/(.*?)\".*?>") of
                {match, [_, {Start, Len}]} ->
                    list_to_integer(lists:sublist(Content, Start + 1, Len));
                ReReason ->
                    ?PRINT("Find Roomid Error, Reason: ~p~n", [ReReason]),
                    error
            end;
        Reason ->
            ?PRINT("Request Error Reason: ~p~n", [Reason]),
            error
    end.