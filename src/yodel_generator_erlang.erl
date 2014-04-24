%%%
%%% Yodel

-module(yodel_generator_erlang).
-include_lib("yodel.hrl").

-import(yodel_utils, [log/3, join/2, to_list/1]).
-import(yodel_tree,  [type_name/1]).

-export([generate/1]).
-export([generated_files/1]).

%%

generate(Tree) ->
    hrl(Tree) ++ erl(Tree).

generated_files(Name) ->
    [hrl_file_name(Name), erl_file_name(Name)].

%% Erlang header file.
hrl_file_name(Name) ->
    output_file_name_prefix(Name) ++ ".hrl".

hrl(#g_tree{module=#g_module{name=Name, exprs=Exprs}}) ->
    Module = atoml(Name),
    FileName = hrl_file_name(Name),
    FileData =
        ifdef_guard(Module, join("\n",
            [expr(Expr) || Expr <- Exprs]
        )),
    [{FileName, FileData}].

ifdef_guard(Module, Body) ->
    [
        "-ifndef(_", Module, "_included).\n",
        "-define(_", Module, "_included, yeah).\n",
        "-include_lib(\"yodel/include/base_types.hrl\")."
        "\n",
        Body,
        "\n",
        "-endif.\n"
    ].

include(Module) ->
    ["-include(\"", Module, ".hrl\").\n"].

expr(Expr = #g_record{} ) -> record(Expr);
expr(Expr = #g_typedef{}) -> typedef(Expr);
expr(Expr = #g_variant{}) -> variant(Expr);
expr(Expr               ) -> log(warn, "Skipping unknown expr ~p", [Expr]), [].

record(#g_record{name=Name, fields=Fields}) ->
    [
        "%% ", Name, "\n",
        "-record(", atoml(type_name(Name)), ", {", join(",", [record_field(F) || F <- Fields]), "\n}).\n",
        "-type(", type(Name), " :: ", "#", atoml(type_name(Name)), "{}", ").\n"
    ].

record_field(#g_record_field{name=Name, type=Type}) ->
    ["\n\t", atoml(Name), " :: ", type(Type)].

typedef(#g_typedef{name=Name, type=Value}) ->
    [
        "%% ", Name, "\n",
        "-type(", type(Name), " :: ", type(Value), ").\n"
    ].

variant(#g_variant{name=Name, values=Values}) ->
    [
        "%% ", Name, "\n",
        "-type(", type(Name), " :: ", join(" | ", [variant_value(V) || V <- Values]), ").\n"
    ].

variant_value(#g_variant_value{tag=Tag, params=[]}) ->
    atoml(Tag);
variant_value(#g_variant_value{tag=Tag, params=Params}) ->
    ["{", atoml(Tag), ", ", join(" | ", [type(P) || P <- Params]), "}"].


%% Erlang implementation file.
erl_file_name(Name) ->
    output_file_name_prefix(Name) ++ ".erl".

erl(#g_tree{module=#g_module{name=Name, exprs=Exprs}}) ->
    Module = atoml(Name),
    FileName = output_file_name_prefix(Name) ++ ".erl",
    [{FileName, [
        "-module(", Module, ").\n",
        include(Module),
        "\n",
        "%% API\n"
        "-export([encode/2, decode/2]).\n",
        "-export([diff/2, patch/2]).\n",
        "\n",
        "%% API\n"
        "encode(Type, Value) ->\n",
        "\tencode_(Type, Value).\n",
        "\n",
        "decode(Type, Data) ->\n",
        "\tdecode_(Type, Data).\n"
        "\n",
        "%% encoders\n",
        encoders(Exprs),
        "\n",
        "%% decoders\n",
        decoders(Exprs),
        "\n",
        "\n",
        "%% utils\n",
        % "-compile({inline,[find_field/2]}).\n",
        "-compile({nowarn_unused_function, find_field/2}).\n",
        "find_field(Field, {PL}) -> proplists:get_value(Field, PL).\n",
        "\n",
        % "-compile({inline,[find_variant_tag/2]}).\n",
        "-compile({nowarn_unused_function, find_variant_tag/2}).\n",
        "find_variant_tag(Field, {PL}) -> proplists:get_value(Field, PL);\n",
        "find_variant_tag(_, V) -> V.\n",
        "\n",
        % "-compile({inline,[required/1]}).\n",
        "-compile({nowarn_unused_function, required/1}).\n",
        "required(undefined) -> error(required_field_is_absent);\n",
        "required(V) -> V.\n",
        "\n",
        % "-compile({inline,[optional/1]}).\n",
        "-compile({nowarn_unused_function, optional/1}).\n",
        "optional(V) -> V.\n",
        "\n",
        "%% diff\n",
        diff(Exprs),
        "\n",
        "%% patch\n",
        patch(Exprs)
    ]}].


%% encoding
encoders(Exprs) ->
    [
        [encode_type(Expr) || Expr <- Exprs], "\n",
        "encode_(string, String) ->\n"
        "\tiolist_to_binary(String);\n"
        "encode_(boolean, Boolean) when is_boolean(Boolean) ->\n"
        "\tBoolean;\n"
        "encode_(integer, Integer) when is_integer(Integer) ->\n"
        "\tInteger;\n"
        "encode_(float, Float) when is_float(Float) or is_integer(Float) ->\n"
        "\tFloat;\n"
        "encode_(binary, Binary) when is_binary(Binary) ->\n"
        "\tBinary;\n"
        "encode_({list, [A]}, List) when is_list(List) ->\n"
        "\t[encode_(A, E) || E <- List];\n"
        "encode_({map, [A, B]}, Map) when is_list(Map) ->\n"
        "\t{[{encode_(A, K), encode_(B, V)} || {K, V} <- Map]};\n"
        "\n"
        "encode_(Type, Value) ->\n"
        "\terror(badarg, [Type, Value]).\n"
        "\n"
        "encode_(Type, Value, Requirement, Acc) -> encode_(undefined, Type, Value, Requirement, Acc).\n"
        "encode_(_Tag     , _Type, undefined, optional, Acc) -> Acc;\n"
        "encode_(_Tag     , _Type, undefined, required, _  ) -> error(required_field_is_absent);\n"
        "encode_(undefined, Type , Value    , _       , Acc) -> [encode_(Type, Value) | Acc];\n"
        "encode_(Tag      , Type , Value    , _       , Acc) -> [{Tag, encode_(Type, Value)} | Acc].\n"
        "\n"
    ].

encode_type(#g_record{name=Name, fields=[]}) ->
    [
        "encode_(", type_tag(Name), ", {", atoml(Name), "}) ->\n",
        "\t{[]};"
    ];
encode_type(#g_record{name=Name, fields=Fields}) ->
    [
        "encode_(", type_tag(Name), ", ",
        "{", atoml(Name), ", ", join(", ", [type_var(FName) || #g_record_field{name=FName} <- Fields]), "}) ->\n",
            "\t{\n",
                join("\n", [
                    ["\t\tencode_(", binary(FName), ", ", type_tag(FType), ", ", type_var(FName), ", ", to_list(FReq), ","] ||
                        #g_record_field{name=FName, requirement=FReq, type=FType} <- Fields
                    ]), " []", lists:duplicate(length(Fields), $)), "\n",
            "\t};\n"
    ];
encode_type(#g_typedef{name=Name, type=Type}) ->
    [
        "encode_(", type_tag(Name), ", ", type_var(Name), ") ->\n",
        "\tencode_(", type_tag(Type), ", ", type_var(Name), ");\n"
    ];
encode_type(#g_variant{name=Name, values=Values}) ->
    [encode_type_variant_value(Name, V) || V <- Values];
encode_type(V) ->
    log(warn, "Skipping unknown expr ~p", [V]),
    [].


encode_type_variant_value(Name, #g_variant_value{tag=Tag, params=[]}) ->
    [
        "encode_(", type_tag(Name), ", ", atoml(Tag), ") ->\n",
        "\t", binary(Tag), ";\n"
    ];
encode_type_variant_value(Name, #g_variant_value{tag=Tag, params=Params}) ->
    [
        "encode_(", type_tag(Name), ", ",
        "{", atoml(Tag), [[", A", to_list(N)] || N <- lists:seq(1, length(Params))], "}) ->\n",
        "\t{[{<<\"tag\">>, ", binary(Tag), "},\n",
        "\t\t{<<\"params\">>,\n" ,
            join("\n", [
                ["\t\t\tencode_(", type_tag(lists:nth(N, Params)), ", ", "A", to_list(N), ", required,"]
                  || N <- lists:seq(1, length(Params))
            ]), " []", lists:duplicate(length(Params), $)), "\n",
        "\t\t}\n"
        "\t]};\n"
    ].


%% decoding
decoders(Exprs) ->
    [
        [decode_type(Expr) || Expr <- Exprs], "\n",
        "decode_(string, String) when is_binary(String) ->\n"
        "\tString;\n"
        "decode_(boolean, Boolean) when is_boolean(Boolean) ->\n"
        "\tBoolean;\n"
        "decode_(integer, Integer) when is_integer(Integer) ->\n"
        "\tInteger;\n"
        "decode_(float, Float) when is_float(Float) or is_integer(Float) ->\n"
        "\tFloat;\n"
        "decode_(binary, Binary) when is_binary(Binary) ->\n"
        "\tBinary;\n"
        "decode_({list, [A]}, List) when is_list(List) ->\n"
        "\t[decode_(A, E) || E <- List];\n"
        "decode_({map, [A, B]}, {Map}) when is_list(Map) ->\n"
        "\t[{decode_(A, K), decode_(B, V)} || {K, V} <- Map];\n"
        "\n"
        "decode_(Type, Data) ->\n"
        "\terror(badarg, [Type, Data]).\n"
        "\n"
        "decode_(_Type, undefined, optional) -> undefined;\n"
        "decode_(_Type, undefined, required) -> error(required_field_is_absent);\n"
        "decode_(Type , Value    , _       ) -> decode_(Type, Value).\n"
        "\n"
    ].

decode_type(#g_record{name=Name, fields=Fields}) ->
    [
        "decode_(", type_tag(Name), ", ", case Fields of [] -> "_"; _ -> "" end, type_var(Name), ") ->\n",
        "\t#", atoml(Name), "{\n",
        join(",\n", [
            ["\t\t", atoml(FName), " = decode_(", type_tag(FType), ", ", "find_field(", binary(FName), ", ", type_var(Name), "), ", to_list(FReq), ")"]
            || #g_record_field{name=FName, requirement=FReq, type=FType} <- Fields
        ]), "\n",
        "\t};\n"
    ];
decode_type(#g_typedef{name=Name, type=Type}) ->
    [
        "decode_(", type_tag(Name), ", ", type_var(Name), ") ->\n",
        "\tdecode_(", type_tag(Type), ", ", type_var(Name), ");\n"
    ];
decode_type(#g_variant{name=Name, values=Values}) ->
    [
        "decode_(", type_tag(Name), ", ", type_var(Name), ") ->\n",
        "\tcase find_variant_tag(<<\"tag\">>, ", type_var(Name), ") of\n",
        [decode_type_variant_value(Name, V) || V <- Values],
        "\t\t_ ->\n",
        "\t\t\terror(badarg, [", type_var(Name), "])\n",
        "\tend;\n"
    ];
decode_type(V) ->
    log(warn, "Skipping unknown expr ~p", [V]),
    [].


decode_type_variant_value(_, #g_variant_value{tag=Tag, params=[]}) ->
    [
        "\t\t", binary(Tag), " ->\n",
        "\t\t\t", atoml(Tag), ";\n"
    ];
decode_type_variant_value(Name, #g_variant_value{tag=Tag, params=Params}) ->
    [
        "\t\t", binary(Tag), " ->\n",
        "\t\t\tPL = find_field(<<\"params\">>, ", type_var(Name), "),\n",
        "\t\t\tlist_to_tuple([", atoml(Tag), "|\n",
        "\t\t\t\tlists:map(\n",
        "\t\t\t\t\tfun({Type, Value}) ->\n",
        "\t\t\t\t\t\tdecode_(Type, required(Value))\n",
        "\t\t\t\t\tend, lists:zip([", join(", ", lists:map(fun type_tag/1, Params)), "], PL))]);\n"
    ].

%% utils
%% представляет эрланг тип для g-типа
%% как для параметризованных, так и конкретных типов
type(<<"String">>        ) -> "string() | binary()";
type(<<"Float">>         ) -> "number()";
type({<<"Map">> , [K, V]}) -> ["list({", type(K), ",", type(V), "})"];
type({<<"Map">> , M     }) -> exit({bad_map, M});
type({<<"List">>, [E]   }) -> ["list(", type(E), ")"];
type({<<"List">>, L     }) -> exit({bad_list, L});
type({<<"Diff">>, [E]   }) -> ["diff(", type(E), ")"];
type({<<"Diff">>, L     }) -> exit({bad_diff, L});
type({Type      , Params}) -> exit({bad_type, {Type, Params}});
type(Type) ->
    [atoml(Type), "()"].

%% представляет тэг для g-типа, которые передаются в ф-цию сериализации
type_tag({Type, Params}) -> ["{", atoml(Type), ", [", join(", ", [type_tag(Param) || Param <- Params]), "]}"];
type_tag(Type) ->
    atoml(Type).

type_var(Name) ->
    var(type_name(Name)).

%% представление базовых элементов эрланга (атом, стока, бинарь, "переменная")
%% атом с переводом первой буквы в lower-case
atoml      (Str) when is_binary(Str) -> yodel_utils:lower(Str).
binary     (Str) when is_binary(Str) -> ["<<\"", Str, "\">>"].
var        (Str) when is_binary(Str) -> yodel_utils:upper(Str).
module_name(Str) when is_binary(Str) -> atoml(Str).

output_file_name_prefix(Name) ->
    to_list(module_name(Name)).

%% diff
diff(_Exprs) ->
    ["diff(_,_) -> ok.\n"].

patch(_Exprs) ->
    ["patch(_,_) -> ok.\n"].
