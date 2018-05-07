% date: 2018-5-7 10:36:56
% author: shaocong_mo@163.com

-module(l_save).

-define(DIC_USER, dic_user).
-define(DIC_USER_CAR, dic_user_car).

-define(WITCHOUT_SAVE_DB, 0).
-define(SAVE_DB, 1).

-record(user_other, {sex = undfined,
                     db_sign = ?WITCHOUT_SAVE_DB}).
-record(user, {id = undefined,
               name = undfined,
               age = 0,
               other_data = #user_other{}}).

-record(user_car, {user_id = undfined,
                   car_type = undefined,
                   db_sign = ?WITCHOUT_SAVE_DB}).

% save list example:
%     {DicName, Field1, Field2}
%     {DicName, Field}
%     the last field must be marked of database
-define(SAVE_LIST, [{?DIC_USER, #user.other_data, #user_other.db_sign},
                    {?DIC_USER_CAR, #user_car.db_sign}]).
%% ====================================================================
%% API functions
%% ====================================================================
-export ([save/0]).

save() ->
    save_1(?SAVE_LIST),
    skip.

save_1([]) ->
    ok;
save_1([{DicName, Field1, Field2} | L]) ->
    save_2(DicName, Field1, Field2),
    save_1(L);
save_1([{DicName, Field} | L]) ->
    save_3(DicName, Field),
    save_1(L).

save_2(DicName, Field1, Field2) ->
    case get_dic(DicName) of
        Dic when is_list(Dic) ->
            save_list(DicName, Dic, Field1, Field2);
        Dic ->
            save_record(DicName, Dic, Field1, Field2)
    end.

save_3(DicName, Field) ->
    case get_dic(DicName) of
        Dic when is_list(Dic) ->
            save_list(DicName, Dic, Field);
        Dic ->
            try
                IsRecord = is_record(Dic, get_record_name(Dic)),
                case IsRecord of
                    true ->
                        save_record(DicName, Dic, Field);
                    _ ->
                        skip
                end
            catch
                _:Reason ->
                    io:format("Error Reason: ~p~n", [Reason]),
                    skip
            end
    end.


save_list(DicName, List, Field) ->
    {NewList, DbList} = save_list_1(List, Field, [], []),
    put(DicName, NewList),
    case DbList of
        [] ->
            skip;
        [H | _] ->
            db_tool:update_list(DbList, get_record_name(H))
    end.

save_list_1([], _, NewList, DbList) ->
    {NewList, DbList};
save_list_1([H | L], Field, NewList, DbList) ->
    case element(Field, H) of
        ?SAVE_DB ->
            NewH = erlang:setelement(Field, H, ?WITCHOUT_SAVE_DB),
            save_list_1(L, Field, [NewH | NewList], [NewH | DbList]);
        _ ->
            save_list_1(L, Field, [H | NewList], DbList)
    end.

save_list(DicName, List, Field1, Field2) ->
    {NewList, DbList} = save_list_1(List, Field1, Field2, [], []),
    put(DicName, NewList),
    case DbList of
        [] ->
            skip;
        [H | _] ->
            db_tool:update_list(DbList, get_record_name(H))
    end.

save_list_1([], _, _, NewList, DbList) ->
    {NewList, DbList};
save_list_1([H | L], Field1, Field2, NewList, DbList) ->
    Other = element(Field1, H),
    case element(Field2, Other) of
        ?SAVE_DB ->
            NewOther = erlang:setelement(Field2, Other, ?WITCHOUT_SAVE_DB),
            NewH = erlang:setelement(Field1, Field1, NewOther),
            save_list_1(L, Field1, Field2, [NewH | NewList], [NewH | DbList]);
        _ ->
            save_list_1(L, Field1, Field2, [H | NewList], DbList)
    end.

save_record(DicName, Record, Field) ->
    case element(Field, Record) of
        ?SAVE_DB ->
            NewRecord = erlang:setelement(Field, Record, ?WITCHOUT_SAVE_DB),
            db_tool:update_one(NewRecord, get_record_name(NewRecord));
        _ ->
            NewRecord = Record
    end,
    put(DicName, NewRecord).

save_record(DicName, Record, Field1, Field2) ->
    Other = element(Field1, Record),
    case element(Field2, Other) of
        ?SAVE_DB ->
            NewOther = erlang:setelement(Field2, Other, ?WITCHOUT_SAVE_DB),
            NewRecord = erlang:setelement(Field1, Record, NewOther),
            db_tool:update_one(NewRecord, get_record_name(NewRecord));
        _ ->
            NewRecord = Record
    end,
    put(DicName, NewRecord).

%% ====================================================================
%% Internal functions
%% ====================================================================

get_record_name(Record) ->
    %% In the record, the first for the record name
    erlang:element(1, Record).

get_dic(DicName) ->
    case get(DicName) of
        undefined ->
            [];
        Info ->
            Info
    end.