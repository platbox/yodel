-ifndef(_yodel_included).
-define(_yodel_included, yeah).


%% Root exprs
-record(g_tree, {module = [] :: g_module()}).
-type(g_tree() :: #g_tree{}).

-type(g_module_name() :: binary()).

-record(g_module, {name :: g_module_name(), exprs::g_exprs()}).
-type(g_module() :: #g_module{}).

-type(g_expr() :: g_variant() | g_typedef() | g_record()).
-type(g_exprs() :: list(g_expr())).


%% Types
-type(g_label() :: binary()).
-type(g_type_name() :: binary()).
-type(g_type() ::
       g_type_name() | g_label()
    | {g_type_name() | g_label(), list(g_type())}
).
-type(g_types() :: list(g_type())).

-type(g_variant_tag() :: binary()).


-record(g_variant, {name::g_type_name(), values::g_variant_values()}).
-type(g_variant() :: #g_variant{}).

-record(g_variant_value, {tag::g_variant_tag(), params::list(g_type())}).
-type(g_variant_value() :: #g_variant_value{}).
-type(g_variant_values() :: list(g_variant_value())).


-record(g_record, {name::g_type_name(), fields::list(g_record_field())}).
-type(g_record() :: #g_record{}).

-type(requirement() :: (required | optional)).
-record(g_record_field, {requirement::requirement(), name::g_label(), type::g_type()}).
-type(g_record_field() :: #g_record_field{}).

-record(g_typedef, {name::g_type(), type::g_type()}).
-type(g_typedef() :: #g_typedef{}).

-endif.