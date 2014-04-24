%%%
%%% Yodel
%%% Core

-module(yodel).

-export([compile_erlang/1]).
-export([generated_files_erlang/1]).
-export([main/1]).

-export([get_option/1]).
-export([get_option/2]).

-import(yodel_utils, [check_success/2, log/3, set_log_level/2]).

compile_erlang(File) ->
    yodel_generator_erlang:generate(yodel_parser:parse(File)).

generated_files_erlang(File) ->
    yodel_generator_erlang:generated_files(yodel_parser:get_module_name(File)).

main(Args) ->
    OptSpecList =
    [
        {verbose      , $v       , "verbose"   , undefined         , "Verbose mode (debug will be printed)."},
        {quite        , $q       , "quite"     , undefined         , "Quite mode (only errros will be printed)."},
        {module       , $m       , "module"    , string            , "Additional erlang source files, which can be used as a generator and/or by a generator (by option for example)."},
        {dry_mode     , $d       , "dry_mode"  , undefined         , "Generate code to memory only (can be useful with verbose fo debugging)."},
        {output_dir   , $o       , "output_dir", {string, "."}     , "Output direcory."},
        {input_file   , undefined, undefined   , string            , "Input file."},
        {generator    , undefined, undefined   , string            , "Output code generator, can be 'module' or 'module:function'."}
    ],
    try
        {Options, GeneratorArgs} = check_success(
            getopt:parse(OptSpecList, Args),
            invalid_run_options
        ),
        set_log_level(
            proplists:get_value(quite  , Options, false),
            proplists:get_value(verbose, Options, false)
        ),

        File = proplists:get_value(input_file, Options),
        (File /= undefined) orelse throw(file_is_undefined),
        Generator = proplists:get_value(generator, Options),
        (Generator /= undefined) orelse throw(generator_is_undefined),

        load_modules(Options),
        write_files(
            proplists:get_value(output_dir, Options),
            generate(
                parse_generator_name(Generator),
                parse_generator_options(GeneratorArgs),
                parse_input_file(File)
            ),
            proplists:get_value(dry_mode, Options, false)
        )
    catch
        throw:Error -> print_error(Error, OptSpecList)
    end.

print_error(Error, OptSpecList) ->
    log(error, "~p", [Error]),
    getopt:usage(OptSpecList, ?MODULE_STRING, "[option_name=option_value] [...]").


parse_generator_options(Args) ->
    [parse_generator_option(Arg) || Arg <- Args].

parse_generator_option(Arg) ->
    case string:tokens(Arg, "=") of
        [Key, Value] -> {Key, Value};
        _ -> throw({invalid_generator_option, Arg})
    end.

make_generator_name(Name) ->
    list_to_atom("yodel_generator_" ++ Name).


parse_generator_name(Generator) ->
    case string:tokens(Generator, ":") of
        [Module          ] -> {make_generator_name(Module), generate};
        [Module, Function] -> {make_generator_name(Module), list_to_atom(Function)}
    end.


%% write output files
write_files(Path, Files, DryModeFlag) ->
    [write_file(filename:join(Path, Name), Data, DryModeFlag) || {Name, Data} <- Files].

write_file(Name, Data, DryModeFlag) ->
    log(debug, "Generated file: ~s~n~s", [Name, binary_to_list(iolist_to_binary(Data))]),
    case DryModeFlag of
        false ->
            log(info , "Writing generated file ~s...", [Name]),
            ok = check_success(
                file:write_file(Name, Data),
                {output_file_saving_failed, Name}
            );
        true ->
            ok
    end.


%% additional modules loading
load_modules(Options) ->
    Files = [File || {module, File} <- Options],
    [load_module(File) || File <- Files].

load_module(File) ->
    log(info, "Compiling module ~s...", [File]),
    {Module, Binary} = check_success(
        compile:file(File, [binary, return_errors, report_errors, report_warnings]),
        module_compilation_error
    ),
    case code:load_binary(Module, File, Binary) of
        {module, Module} -> ok;
        {error, Reason} -> throw({module_loading_error, File, Reason})
    end.


%% input file parsing
parse_input_file(FileName) ->
    log(info, "Parsing input file ~s...", [FileName]),
    Tree = yodel_parser:parse(FileName),
    log(debug, "GDT tree: ~n~p", [Tree]),
    Tree.


%% code generating
generate({M, F}, Optinos, Tree) ->
    log(debug, "Generator options is: ~n~p", [Optinos]),
    set_options(Optinos),
    log(info, "Generating code using ~p:~p function...", [M,F]),
    M:F(Tree).


set_options(Options) ->
    put(generator_options, Options).

get_option(Key) ->
    get_option(Key, undefined).

get_option(Key, Default) ->
    Options = get(generator_options),
    proplists:get_value(Key, Options, Default).
