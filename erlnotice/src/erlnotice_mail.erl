-module (erlnotice_mail).
  
-behaviour(gen_server).  
% --------------------------------------------------------------------  
% Include files  
% --------------------------------------------------------------------  
-include ("erlnotice.hrl").
% --------------------------------------------------------------------  
% External exports  
% --------------------------------------------------------------------  
-export([start_link/3]).  
  
% gen_server callbacks  
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).  
  
-record(state, {account = undefined,
				password = undefined,
				smtp = undefined}).  

-define(LOOP_TIME, 1000 * 10).
-define(A_DAY_SECONDS, (24*60*60)).
-define(JIJIN_NOTICE_TIME, 15 * 60*60).
% --------------------------------------------------------------------  
% External API  
% --------------------------------------------------------------------  
start_link(Account, Password, Smtp) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [Account, Password, Smtp], []).

% --------------------------------------------------------------------  
% Function: init/1  
% Description: Initiates the server  
% Returns: {ok, State}          |  
%          {ok, State, Timeout} |  
%          ignore               |  
%          {stop, Reason}  
% --------------------------------------------------------------------  
init([Account, Password, Smtp]) ->  
	erlang:send_after(?LOOP_TIME, self(), {loop}),
    {ok, #state{account = Account,
    			password = Password,
    			smtp = Smtp}}.  
  
% --------------------------------------------------------------------  
% Function: handle_call/3  
% Description: Handling call messages  
% Returns: {reply, Reply, State}          |  
%          {reply, Reply, State, Timeout} |  
%          {noreply, State}               |  
%          {noreply, State, Timeout}      |  
%          {stop, Reason, Reply, State}   | (terminate/2 is called)  
%          {stop, Reason, State}            (terminate/2 is called)  
% --------------------------------------------------------------------  
handle_call(_Request, _From, State) ->  
    Reply = ok,  
    {reply, Reply, State}.  
  
% --------------------------------------------------------------------  
% Function: handle_cast/2  
% Description: Handling cast messages  
% Returns: {noreply, State}          |  
%          {noreply, State, Timeout} |  
%          {stop, Reason, State}            (terminate/2 is called)  
% --------------------------------------------------------------------  
handle_cast(_Msg, State) ->  
    {noreply, State}.  
  
% --------------------------------------------------------------------  
% Function: handle_info/2  
% Description: Handling all non call/cast messages  
% Returns: {noreply, State}          |  
%          {noreply, State, Timeout} |  
%          {stop, Reason, State}            (terminate/2 is called)  
% --------------------------------------------------------------------  
handle_info({loop}, State) ->
	erlang:send_after(?LOOP_TIME, self(), {loop}),
	{MegaSecs, Secs, _} = os:timestamp(),
	Seconds = MegaSecs*1000*1000+Secs,
	TodaySeconds = Seconds - Seconds div ?A_DAY_SECONDS * ?A_DAY_SECONDS + 8*3600,
	Notice =  TodaySeconds == ?JIJIN_NOTICE_TIME,
	case Notice of
		true ->
			send_jijin_notice_mail(State);
		 _ ->
		 	skip
	end,
	send_jijin_notice_mail(State),
	{noreply, State};

handle_info(_Info, State) ->  
    {noreply, State}.  
  
% --------------------------------------------------------------------  
% Function: terminate/2  
% Description: Shutdown the server  
% Returns: any (ignored by gen_server)  
% --------------------------------------------------------------------  
terminate(_Reason, _State) ->  
    ok.  
  
% --------------------------------------------------------------------  
% Func: code_change/3  
% Purpose: Convert process state when code is changed  
% Returns: {ok, NewState}  
% --------------------------------------------------------------------  
code_change(_OldVsn, State, _Extra) ->  
    {ok, State}.   


send_notice_mail(State, Subject, From, To, Body) ->
	Mail = lists:append(["Subject: ", Subject, "\r\nFrom: ", From, "\r\nTo: ", To, "\r\n\r\n", Body]),
	send_mail(State#state.account, State#state.password, State#state.smtp, Mail).

send_mail(Account, Password, Smtp, Mail) ->
	gen_smtp_client:send({Account, [Account], Mail},
  						 [{relay, Smtp}, {username, Account}, {password, Password}]).

send_jijin_notice_mail(State) ->
	JiJins = ["161032"],
	send_jijin_notice_mail(JiJins, State).

send_jijin_notice_mail([], _) ->
	skip;
send_jijin_notice_mail([Code | L], State) ->
	Url = lists:append(["http://fundgz.1234567.com.cn/js/", Code, ".js"]),
	case httpc:request(Url) of
		{ok, {_, _, Content}} ->
			List = binary:split(erlang:list_to_binary(lists:sublist(Content, 10, length(Content) - 13)),
								<<$,>>, [global, trim]),
			NewList = send_jijin_notice_mail_2(List, []),
			{_, Name} = lists:keyfind(name, 1, NewList),
			{_, Code} = lists:keyfind(fundcode, 1, NewList),
			{_, Gszzl} = lists:keyfind(gszzl, 1, NewList),
			SendTitle = lists:append([Code, " status:", Gszzl, "%"]),
			SendContent = lists:append([Name, " status:", Gszzl, "%"]),
			send_notice_mail(State, SendTitle, "notice", "me", SendContent);
		_ ->
			skip
	end,
	send_jijin_notice_mail(L, State).

send_jijin_notice_mail_2([], NewList) ->
	NewList;
send_jijin_notice_mail_2([H | L], NewList) ->
	case binary:split(H, <<$:>>) of
		[Key, Value] ->
			[_, Key1] = binary:split(Key, <<"\"">>, [global, trim]),
			[_, Value1] = binary:split(Value, <<"\"">>, [global, trim]),
			send_jijin_notice_mail_2(L, [{erlang:binary_to_atom(Key1, latin1), erlang:binary_to_list(Value1)} | NewList]);
		_ ->
			send_jijin_notice_mail_2(L, NewList)
	end.