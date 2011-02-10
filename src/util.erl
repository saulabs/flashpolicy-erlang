-module(util).

-export([info/1, refresh/1, refresh_test/1]).


%% prints server infos
info(memory) ->
  CalculateMemoryFun = fun(Pg2Group, GroupInfo) -> 
    ProcessesInGroup = pg2:get_members(Pg2Group),
    Bytes = lists:foldl(fun(Pid, Acc) -> 
      {memory, Memory} = erlang:process_info(Pid, memory),
      Acc + Memory
    end, 0.0, ProcessesInGroup),
    NumProcesses = length(ProcessesInGroup),
    BytesPerProcess = if NumProcesses > 0 -> trunc(Bytes / NumProcesses); true -> 0.0 end,
    {Bytes, BytesPerProcess, NumProcesses, GroupInfo}
  end,
  ProcessGroups = [],
  MemoryUsage = [CalculateMemoryFun(GroupName, GroupDescription) || {GroupName, GroupDescription} <- ProcessGroups], 
  MemoryUsageSorted = lists:reverse(lists:keysort(1, MemoryUsage)),
  [io:format("~4.B ~s Processes consume ~s / ~s per process~n", [NumProcesses, GroupInfo, format_bytes(Bytes), format_bytes(BytesPerProcess)]) 
    || {Bytes, BytesPerProcess, NumProcesses, GroupInfo} <- MemoryUsageSorted],

  {SumBytes, SumProcesses} = lists:foldl(fun({Bytes, _BytesPerProcess, NumProcesses, _GroupInfo}, {BytesAcc, ProcessesAcc}) -> 
    {BytesAcc + Bytes, ProcessesAcc + NumProcesses}
  end, {0.0, 0}, MemoryUsage),
  SumBytesPerProcess = if SumProcesses > 0 -> trunc(SumBytes / SumProcesses); true -> 0.0 end,
  io:format("-------------------------------------------------~n"),
  io:format("~4.B checked         Processes consume ~s / ~s per process~n", [SumProcesses, format_bytes(SumBytes), format_bytes(SumBytesPerProcess)]); 
  
info(_) -> unknown.

format_bytes(NumBytes) ->
  {Amount, Unit} = if
    NumBytes > 1000000000 -> {NumBytes/1000000000, "gb"};
    NumBytes > 1000000    -> {NumBytes/1000000, "mb"};
    NumBytes > 1000       -> {NumBytes/1000, "kb"};
    true                  -> {NumBytes, "b "}
  end,
  io_lib:format("~7.2f ~s", [Amount, Unit]).

refresh(Module) when is_atom(Module) ->
  refresh(Module, get_processes(Module), []).

refresh_test(Module) ->
  refresh(Module, get_processes(Module), [{d,'TEST',true}, export_all]).

%% refreshes a single module
refresh(Module, Processes, ExtraOptions) when is_atom(Module) ->
  io:format("~ncompiling module '~w':~n",[Module]),
  case compile:file("src/" ++ atom_to_list(Module) ++ ".erl", [verbose,report_errors,report_warnings, {i, "./include"}, {outdir, "./ebin"}] ++ ExtraOptions) of
    {ok, Module} ->
      [catch sys:suspend(Pid) || Pid <- Processes],
      code:purge(Module),
      case code:load_file(Module) of
        {module, Module} ->
          io:format("~nSUCCESSFULLY compiled and reloaded module '~w'",[Module]),
          if length(Processes) > 0 ->
            io:format("~nSending code change event to ~B processes~n",[length(Processes)]),
            [catch sys:change_code(Pid, Module, foo, foo) || Pid <- Processes];
          true -> ok
          end;
        ReloadError ->
          io:format("~nFAILED to reload module '~w': ~w",[Module, ReloadError])
      end,
      [catch sys:resume(Pid) || Pid <- Processes];
    CompileError ->
      io:format("~nFAILED to compile module '~w': ~w",[Module, CompileError])
  end.

get_processes(_Module) ->
  [].


  