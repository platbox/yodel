%%%
%%% Yodel

-module(yodel_parser).
-include_lib("yodel.hrl").

-export([parse/1]).
-export([get_module_name/1]).


%% parse yaml datamodel
parse(File) ->
    {ok, [Yaml]} = yaml:load_file(File, []),
    Name = get_module_name(File),
    #g_tree{module=#g_module{name=parse_module_name(Name), exprs=parse_exprs(Yaml)}}.

get_module_name(Filename) ->
    Base = filename:basename(Filename),
    ModName =
        case Base of
            Str when is_list(Str) ->
                [H|_] = string:tokens(Base, "."),
                list_to_binary(H);
            Bin when is_binary(Bin) ->
                [H|_] = binary:split(Base, <<".">>),
                H
        end,
    yodel_utils:upper(ModName).

%% local
parse_exprs(Exprs) ->
    [parse_expr(E) || E <- Exprs].

parse_expr([{<<"typedef">>, Typedef}]) ->
    [{<<"name">>, Name}, {<<"type">>, Type}] = sort(Typedef),
    #g_typedef{name=parse_type_def(Name), type=parse_type_use(Type)};
parse_expr([{<<"record">>, Record}]) ->
    [{<<"fields">>, Fields}, {<<"name">>, Name}] = sort(Record),
    #g_record{name=parse_type_def(Name), fields=parse_list(Fields, fun parse_record_field/1)};
parse_expr([{<<"variant">>, Variant}]) ->
    [{<<"name">>, Name}, {<<"values">>, Values}] = sort(Variant),
    #g_variant{name=parse_type_def(Name), values=parse_list(Values, fun parse_variant_value/1)};
parse_expr(V) ->
    V.

parse_list(List, F) ->
    [F(E) || E <- List].

parse_record_field(Field) ->
    [{<<"name">>, Name}, {<<"req">>, Req}, {<<"type">>, Type}] = sort(Field),
    #g_record_field{name=parse_label(Name), requirement=parse_requirement(Req), type=parse_type_use(Type)}.

parse_requirement(<<"required">>) -> required;
parse_requirement(<<"optional">>) -> optional.

parse_variant_value(Value) ->
    case sort(Value) of
        [{<<"params">>, Params}, {<<"tag">>, Tag}] ->
            #g_variant_value{tag=parse_tag(Tag), params=parse_list(Params, fun parse_type_use/1)};
        [{<<"tag">>, Tag}] ->
            #g_variant_value{tag=parse_tag(Tag), params=[]}
    end.

parse_module_name(M) ->
    check(fun yodel_utils:is_upper/1, M).

parse_tag(T) ->
    check(fun yodel_utils:is_upper/1, T).

parse_label(L) ->
    check(fun yodel_utils:is_lower/1, L).

parse_type_def(V) ->
    check(fun yodel_utils:is_upper/1, V) .

parse_type_use(V) when is_binary(V) ->
    [H|T] =
        lists:filter(fun(E) -> E /= <<>> end, binary:split(V, [<<" ">>, <<"\t">>],[global])),
    case T of
        [] -> H;
        _  -> {H, T}
    end.

check(F, V) ->
    case F(V) of
        true -> V;
        _ -> exit({bad_value, V})
    end.

%% utils
sort(List) ->
    lists:keysort(1, List).
