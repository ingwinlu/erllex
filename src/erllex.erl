%%% =======================================================
%%% @author Lukas Winkler <derwinlu@gmail.com>
%%% @doc erllex showcases erlangs re module / regex lexing and group matching
%%% @end
%%% =======================================================
-module(erllex).
-export([get_rules/0, tokenize/1, tokenize/2]).

-type rule()   :: Regexp::iodata().
-type rules()  :: [Rule::rule()].

-type token()  :: {Groupname::binary(), Value::binary()}.
-type tokens() :: [Token::token()].

%%%--------------------------------------------------------
%%% @doc Returns a set of sample Regexp for the Lexer
%%% @end
%%%--------------------------------------------------------
-spec get_rules() -> Rules::rules().
get_rules() -> 
    [
        "(?P<NUMBER>\\d+)",
        "(?P<IDENTIFIER>[a-zA-Z_]\\w+)",
        "(?P<PLUS>\\+)",
        "(?P<MINUS>\\-)",
        "(?P<WHITESPACE>\\s)"
    ].

%%%--------------------------------------------------------
%%% @equiv tokenize(BinaryString, erllex:get_rules())
%%% @end
%%%--------------------------------------------------------
-spec tokenize(BinaryString::binary()) -> Tokens::tokens().
tokenize(BinaryString) ->
    tokenize(BinaryString, erllex:get_rules()).

%%%--------------------------------------------------------
%%% @doc Tokenize a BinaryString according to Rules and returns Tokens as a List.
%%% @end
%%%--------------------------------------------------------
-spec tokenize(BinaryString::binary(), Rules::rules()) -> Tokens::tokens().
tokenize(BinaryString, Rules) ->
    ConcatRules = concat_rules(Rules),
    {ok, CompiledRules} = re:compile(ConcatRules, [anchored]),
    {namelist, Namelist} = re:inspect(CompiledRules, namelist),
    get_tokens(BinaryString, CompiledRules, Namelist, []).


%%%========================================================
%%% priv
%%%========================================================

get_tokens(<<>>, _Rules, _Namelist, Acc) ->
     lists:reverse(Acc);
get_tokens(BinaryString, Rules, Namelist, Acc) ->
    {match, Matchlist} = re:run(BinaryString,Rules,[{capture,all_names,binary}]),
    {ok, Token} = extract_token(Namelist, Matchlist),
    {_, Matched} = Token,
    MatchedLength = byte_size(Matched),
    <<_Matched:MatchedLength/binary, NewBinaryString/binary>> = BinaryString,
    NewAcc = [Token | Acc],
    get_tokens(NewBinaryString, Rules, Namelist, NewAcc).

concat_rules(NaiveRuleset) ->
    lists:foldl(
        fun(Rule, Acc) ->
            Rule ++ "|" ++ Acc
        end,
        [],
        NaiveRuleset
    ).

extract_token([_Name | Namelist], [<<>> | Matchlist]) ->
    extract_token(Namelist, Matchlist);
extract_token([Name | _Namelist], [Match | _Matchlist]) ->
    {ok, {Name, Match}};
extract_token(_, _) ->
    {error, nomatch}.

