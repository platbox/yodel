%%%
%%% Yodel

-module(yodel_rebar_plugin).

%% rebar plugins callbacks

-export([pre_compile/2]).
% -export([pre_clean/2]).


%% rebar plugins callbacks
pre_compile(Config, _AppFile) ->
    {InDir, OutDir, SrcExt} = get_options(Config),
    F = fun(InputFile) ->
            needs_compile(InputFile, OutDir)
        end,
    Files = lists:filter(F, filelib:wildcard(InDir++"/*"++SrcExt)),
    rebar_base_compiler:run(Config,
                            Files,
                            [],
                            fun(CompiledFile, _) ->
                                compile_datamodel(CompiledFile, OutDir)
                            end).


%% local
compile_datamodel(InputFile, OutDir) ->
    Files = yodel:compile_erlang(InputFile),
    [file:write_file(filename:join(OutDir, Name), Data) || {Name, Data} <- Files],
    ok.

needs_compile(SrcFile, OutDir) ->
    OutputFiles = lists:map(fun(E) -> filename:join(OutDir, E) end, yodel:generated_files_erlang(SrcFile)),
    F = fun(OutputFile) ->
            filelib:last_modified(SrcFile) > filelib:last_modified(OutputFile)
        end,
    lists:any(F, OutputFiles).

get_options(Config) ->
    DMOpts = rebar_config:get_all(Config, yodel_opts),
    % proplists:get_value(yodel_opts, Config, []),
    InDir  = proplists:get_value(input_dir , DMOpts, "yodel"),
    OutDir = proplists:get_value(output_dir, DMOpts, "src"),
    SrcExt = proplists:get_value(source_ext, DMOpts, ".yaml"),
    {InDir, OutDir, SrcExt}.
