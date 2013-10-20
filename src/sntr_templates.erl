-module(sntr_templates).

load(State) ->


load_templates() ->
    Templates = [],
    try load_escript_templates(State, Templates) of

load_priv_dir_templates() ->
    ok.

load_escript_templates() ->
    {ok, Props2} = escript:extract("sinter", []).
