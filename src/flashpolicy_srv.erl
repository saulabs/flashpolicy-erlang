-module(flashpolicy_srv).

-behaviour(gen_server).

-export([start_link/4, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([send_policy/2]).

-record(state, {
  listen_address,
  listen_port,
  server_socket,
  policy,
  accept_pid = none,
  accepted_clients = []
}).


start_link(PolicyFile = [_|_], ListenAddress, Port, LoggingEnabled) when is_integer(Port), is_boolean(LoggingEnabled) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [PolicyFile, ListenAddress, Port, LoggingEnabled], []).
  
init([PolicyFile = [_|_], ListenAddress, Port, LoggingEnabled]) when is_integer(Port), is_boolean(LoggingEnabled) ->
  process_flag(trap_exit, true),

  ListenOptions = [binary, {packet_size, 2048}, {packet, 2}, {backlog, 1024}, {active, false}, {reuseaddr, true}] ++ 
                  case ListenAddress of 
                    {_, _, _, _} -> [{ip, ListenAddress}];
                    _Any -> []
                  end,
                  
  case load_policy_file(PolicyFile) of
    {ok, PolicyContent} ->
      case gen_tcp:listen(Port, ListenOptions) of
        {ok, ServerSocket} ->
          State = #state {
            listen_address = ListenAddress,
            listen_port = Port,
            server_socket = ServerSocket,
            policy = PolicyContent
          },
          StateWithAcceptLoopStated = State#state { accept_pid = spawn_accept_process(State) },
          error_logger:info_report([{message, 'started policy server.'}, {listen_address, ListenAddress}, {port, Port}, {policy, PolicyFile}, {module, ?MODULE}, {line, ?LINE}]),
          {ok, StateWithAcceptLoopStated};
        Error -> % could not listen at port
          {stop, Error}
      end;
    {error, Reason} -> % could not load policy file
      error_logger:error_report([{message, 'could not load policy file.'}, {file, PolicyFile}, {error, Reason}, {module, ?MODULE}, {line, ?LINE}]),
      {stop, Reason}
  end.

stop() ->
  gen_server:cast({local, ?MODULE}, stop).

% accept loop terminated
handle_info({'EXIT', AcceptLoopPid, Reason}, State = #state {accept_pid = AcceptLoopPid})->
  case Reason of 
    normal -> {stop, normal, State#state {accept_pid = none}};
    _ ->
      error_logger:error_report([{message, 'restarting accept loop.'}, {error, Reason}, {module, ?MODULE}, {line, ?LINE}]),
      {noreply, State#state {accept_pid = spawn_accept_process(State)}}
  end;

% client connection terminated
handle_info({'EXIT', ConnectionPid, _Reason}, State = #state{ accepted_clients = AcceptedClients}) ->
  case lists:member(ConnectionPid, AcceptedClients) of 
    true  -> {noreply, State#state { accepted_clients = lists:delete(ConnectionPid, AcceptedClients)}};
    false -> {noreply, State} % connection pid not found
  end;
  
% client connection established  
handle_info({accepted, ConnectionPid}, State = #state{ accepted_clients = AcceptedClients}) ->
  StateWithAcceptedConnection = case catch erlang:link(ConnectionPid) of
    true -> State#state {accepted_clients = [ConnectionPid | AcceptedClients]};
    _    -> State
  end,
  {noreply, StateWithAcceptedConnection};
  
% stop this tcp server
handle_info(stop, State = #state {listen_address = ListenAddress, listen_port = Port, accept_pid = AcceptLoopPid}) ->
  error_logger:info_report([{message, 'stopping tcp server.'}, {listen_address, ListenAddress}, {port, Port}, {module, ?MODULE}, {line, ?LINE}]),
  AcceptLoopPid ! stop,  
  {stop, normal, State};

handle_info(Event, State) ->
  error_logger:info_report([{message, 'received unexpected event in handle_info.'}, {event, Event}, {module, ?MODULE}, {line, ?LINE}]),
  {noreply, State}.

handle_cast(Event, State) ->
  error_logger:info_report([{message, 'received unexpected event in handle_cast.'}, {event, Event}, {module, ?MODULE}, {line, ?LINE}]),
  {noreply, State}.

handle_call(Event, _From, State) ->
  error_logger:info_report([{message, 'received unexpected event in handle_cal.'}, {event, Event}, {module, ?MODULE}, {line, ?LINE}]),
  {reply, error, State}.

terminate(_Reason, #state {server_socket = ServerSocket}) ->
  catch gen_tcp:close(ServerSocket),
  ok.

code_change(_OldVsn, State, _Extra) ->
 {ok, State}.

send_to_client(Socket, PolicyContent) ->
  gen_tcp:send(Socket, PolicyContent). 

send_policy(Socket, PolicyContent) ->
  receive
    became_controlling_process ->
      SocketOptions = [
        {packet, raw},
        {active, true},
        {send_timeout, 15000},
        {send_timeout_close, true},
        {keepalive, true}
      ],
      IP = unknown,
      case inet:setopts(Socket, SocketOptions) of
        ok -> 
          case catch send_to_client(Socket, PolicyContent) of 
            ok              -> error_logger:info_report([{message, 'sent policy to client.'}, {ip, IP}, {module, ?MODULE}, {line, ?LINE}]);
            {error, Reason} -> error_logger:info_report([{message, 'faild to send policy to client.'}, {ip, IP}, {error, Reason}, {module, ?MODULE}, {line, ?LINE}])
          end;
        Error -> % could not send policy
          error_logger:info_report([{message, 'faild to send policy to client.'}, {ip, IP}, {error, Error}, {module, ?MODULE}, {line, ?LINE}])
      end,
      catch gen_tcp:close(Socket);
    stop -> ok
    after 1000 -> ok
  end.

spawn_accept_process(State = #state{}) -> 
 spawn_link(fun() -> accept(State) end).
 
accept(State = #state{server_socket = ServerSocket, policy = PolicyContent}) ->
  case gen_tcp:accept(ServerSocket) of 
    {ok, SocketPid} ->
      ConnectionPid = spawn(?MODULE, send_policy, [SocketPid, PolicyContent]),
      case gen_tcp:controlling_process(SocketPid, ConnectionPid) of
        ok              -> ConnectionPid ! became_controlling_process;
        {error, closed} -> ConnectionPid ! stop;
        {error, Reason} -> ConnectionPid ! stop,
            error_logger:error_report([{message, 'error when setting client connection as controlling process'}, {error, Reason}, {module, ?MODULE}, {line, ?LINE}])
      end,      
      handle_socket_messages(State);
    {error, timeout} ->
      handle_socket_messages(State);
    {error, closed} -> % tcp server closed socket, so terminate
      ok;
    Error ->
      error_logger:error_report([{message, 'unexpected error during accept.'}, {error, Error}, {module, ?MODULE}, {line, ?LINE}]),
      handle_socket_messages(State)
  end.

handle_socket_messages(SocketState) ->
  receive
    Message -> 
      case Message of
        stop   -> stop;
        _Other -> accept(SocketState)
      end
    after 0 ->
      accept(SocketState)
  end.
                
readlines(FileHandle, Acc) ->
  case io:get_line(FileHandle, "") of
    eof  -> file:close(FileHandle), Acc;
    Line -> readlines(FileHandle, [Line | Acc])
  end.
    
load_policy_file(PolicyFile) ->
  case file:open(PolicyFile, [read]) of
    {ok, FileHandle}         -> {ok, lists:flatten(lists:reverse(readlines(FileHandle, []))) ++ [0]};
    Error = {error, _Reason} -> Error;
    Unknown                  -> {error, Unknown}
  end.
