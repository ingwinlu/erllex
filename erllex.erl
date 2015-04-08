-module(erllex).
-export([get_rules/0, tokenize/2]).

get_rules() -> 
    [
        "(?P<NUMBER>\\d+)",
        "(?P<IDENTIFIER>[a-zA-Z_]\\w+)",
        "(?P<PLUS>\\+)",
        "(?P<MINUS>\\-)",
        "(?P<WHITESPACE>\\s)"
    ].

tokenize(BinaryString, Rules) ->
    ConcatRules = concat_rules(Rules),
    {ok, CompiledRules} = re:compile(ConcatRules, [anchored]),
    {namelist, Namelist} = re:inspect(CompiledRules, namelist),
    get_tokens(BinaryString, CompiledRules, Namelist, []).

get_tokens(<<>>, _Rules, _Namelist, Acc) ->
     lists:reverse(Acc);
get_tokens(BinaryString, Rules, Namelist, Acc) ->
    {match, Matchlist} = re:run(BinaryString,Rules,[{capture,all_names,binary}]),
    Token = extract_token(Namelist, Matchlist),
    {_, Matched} = Token,
    MatchedLength = byte_size(Matched),
    <<_Matched:MatchedLength/binary, NewBinaryString/binary>> = BinaryString,
    NewAcc = [Token | Acc],
    get_tokens(NewBinaryString, Rules, Namelist, NewAcc).

%%%PRIV
concat_rules(NaiveRuleset) ->
    lists:foldl(
        fun(Rule, Acc) ->
            Acc ++ Rule ++ "|"
        end,
        [],
        NaiveRuleset
    ).

extract_token([_Name | Namelist], [<<>> | Matchlist]) ->
    extract_token(Namelist, Matchlist);
extract_token([Name | _Namelist], [Match | _Matchlist]) ->
    {Name, Match};
extract_token(_, _) ->
    {error, nomatch}.
