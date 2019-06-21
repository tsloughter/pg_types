-module(pg_numeric).

-behaviour(pg_types).

-export([init/1,
         encode/2,
         decode/2]).

-include("pg_protocol.hrl").

-define(DEC_DIGITS, 4).
-define(POWER_OF_2_TO_52, 4503599627370496).

init(_Opts) ->
    {[<<"numeric_send">>], []}.

encode(Numeric, _) ->
    Data = encode_numeric(Numeric),
    [<<(iolist_size(Data)):?int32>>, Data].

decode(Numeric, _) ->
    decode_numeric_bin(Numeric).

encode_numeric('NaN') ->
    <<0:16/unsigned, 0:16, 16#C000:16/unsigned, 0:16/unsigned>>;
encode_numeric(Float) ->
    {Sign, Coef, Exp} = parse_num(Float),
    {IntDigits, FloatDigits, Scale} = to_digits(Coef, Exp, -Exp),
    Weight = max(length(IntDigits) - 1, 0),
    Digits = IntDigits ++ FloatDigits,
    Len = length(Digits),
    NumSign = encode_sign(Sign),
    numeric_bin(Len, Weight, NumSign, Scale, Digits).

numeric_bin(Len, Weight, Sign, Scale, Digits) ->
    Bin = [<<I:16/unsigned-integer>> || I <- Digits],
    [<<Len:16/integer, Weight:16/integer, Sign:16/unsigned, Scale:16/integer>> | Bin].

fix_float_exp(Digits) ->
    fix_float_exp(Digits, []).

fix_float_exp([$e | Rest], [$0 | [$. | Result]]) ->
    fix_float_exp(Rest, [$e | Result]);

fix_float_exp([Digit | Rest], Result) ->
    fix_float_exp(Rest, [Digit | Result]);

fix_float_exp([], Result) ->
    lists:reverse(Result).

to_binary(Num) when is_float(Num)->
    list_to_binary(fix_float_exp(io_lib_format:fwrite_g(Num)));
to_binary(Num) when is_integer(Num) ->
    list_to_binary(integer_to_list(Num));
to_binary(Num) when is_list(Num) ->
    list_to_binary(Num);
to_binary(Num) when is_binary(Num) ->
    Num.

to_digits(Coef, Exp, Scale) ->
    {IntParts, FloatParts, Scale1} =
        case Scale >= 0 of
            true ->
                Base = pg_math:pow10(Scale),
                Int = Coef div Base,
                Dec = Coef rem Base,
                {Int, Dec, -Exp};
            false ->
                Base = pg_math:pow10(-Scale),
                {Coef * Base, 0, 0}
        end,

    IntDigits = encode_digits(IntParts, []),
    FloatDigits = encode_float(FloatParts, Scale1),
    {IntDigits, FloatDigits, Scale1}.

parse_num(Num) ->
    case string:lowercase(to_binary(Num)) of
        <<Char:1/binary, Rest/binary>> when  Char =:= <<"-">> orelse Char =:= <<"+">> ->
            {Coef, Exp} = parse_unsigned(Rest),
            {parse_sign(Char), Coef, Exp};
        Bin ->
            {Coef, Exp} = parse_unsigned(Bin),
            {1, Coef, Exp}
    end.

parse_sign(<<"-">>) ->
    -1;
parse_sign(_Char) ->
    1.

parse_unsigned(<<"inf">>) ->
    {inf, 0};
parse_unsigned(<<"infinity">>) ->
    {inf, 0};
parse_unsigned(<<"snan">>) ->
    {sNaN, 0};
parse_unsigned(<<"nan">>) ->
    {qNaN, 0};
parse_unsigned(Bin) ->
    {Int, Rest} = parse_digits(Bin, []),
    {Float, Rest1} = parse_float(Rest),
    {Exp, Rest2} = parse_exp(Rest1),

    case has_invalid_parts(Int, Float, Rest2) of
        true ->
            %% TODO: Provide a more meaningful error messasge
            {error, invalid_parts, Int, Float, Rest2};
        false ->
            Coef = list_to_integer(zero_if_empty(Int) ++ Float),
            Exp0 = list_to_integer(zero_if_empty(Exp)) - length(Float),
            { Coef, Exp0 }
    end.

parse_digits(<<Digit, Rest/binary>>, Acc) ->
    case lists:member(Digit, lists:seq($0, $9)) of
        true ->
            parse_digits(Rest, [Digit | Acc]);
        false ->
            {lists:reverse(Acc), << <<Digit>>/binary , Rest/binary >>}
    end;
parse_digits(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>}.

parse_float(<<Char:1/binary,  Rest/binary>> = Bin) ->
    case Char =:= <<".">> of
        true ->
            parse_digits(Rest, []);
        false -> {[], Bin}
    end;
parse_float(<<>>) ->
    {[], <<>>}.

parse_exp(<< Char:1/binary, Rest/binary>>) when Char =:= <<"e">> ->
    <<Sign, R/binary>> = Rest,
    case Sign of
        S when S =:= $- orelse S =:= $+ ->
            {Digits, R1} = parse_digits(R, []),
            {[Sign | Digits], R1};
        _ ->
            parse_digits(Rest, [])
    end;
parse_exp(<<>>) ->
    {[], <<>>}.

encode_digits(Coef, Digits) ->
    case Coef < 10000 of
        true -> [Coef|Digits];
        false -> encode_digits(Coef div 10000, [Coef rem 10000 | Digits])
    end.

encode_float(Float, Scale) ->
    Prefix = prefix_scale(Float, Scale) div ?DEC_DIGITS,
    Suffix = ?DEC_DIGITS - (Scale rem ?DEC_DIGITS),
    Float1 = Float * pg_math:pow10(Suffix),
    lists:duplicate(Prefix, 0) ++ encode_digits(Float1, []).

prefix_scale(Num, Scale) ->
    case Num of
        0 ->
            Scale;
        _ ->
            prefix_scale(Num div 10, Scale - 1)
    end.

encode_sign(Sign) ->
    case Sign  of
        1 -> 16#0000;
        -1 -> 16#4000
    end.

has_invalid_parts(Int, Float, Rest) ->
    (Rest  =/= <<>>) orelse (Int =:= [] andalso Float =:= []).

zero_if_empty(Num) ->
    case Num of
        [] ->
            "0";
        _ ->
            Num
    end.

decode_numeric_bin(<<0:16/unsigned, _Weight:16, 16#C000:16/unsigned, 0:16/unsigned>>) -> 'NaN';

decode_numeric_bin(<<Len:16/unsigned, DWeight:16/signed, Sign:16/unsigned, DScale:16/unsigned, Tail/binary>>) when Sign =:= 16#0000 orelse Sign =:= 16#4000 ->
    Len = byte_size(Tail) div 2,
    {ValueInt, Weight} = decode_numeric_int(Tail, DWeight, 0),
    ValueDec = scale(ValueInt, (Weight + 1) * 4 + DScale),

    OSign = case Sign of
                16#0000 -> 1;
                16#4000 -> -1
            end,

    case {DWeight, DScale} of
        {0, 0} ->
            OSign * ValueDec;
        _ ->
            to_float(OSign, ValueDec, -DScale)
    end.

to_float(Sign, Coef, Scale) ->
    %% See http://www.exploringbinary.com/correct-decimal-to-floating-point-using-big-integers/ for details
    {Num, Den} = case Scale >= 0 of
                     true ->
                         {Coef * pg_math:pow10(Scale), 1};
                     false ->
                         {Coef, pg_math:pow10(-Scale)}
                 end,

    Boundary = Den bsl 52,

    case Num of
        0 ->
            0.0;
        N when N >= Boundary ->
            {NewDen, Exp} = scale_down(N, Boundary, 52),
            decimal_to_float(Sign, N, NewDen, Exp);
        _ ->
            {NewNum, Exp} = scale_up(Num, Boundary, 52),
            decimal_to_float(Sign, NewNum, Den, Exp)
    end.

scale_up(Num, Den, Exp) when Num >= Den ->
    {Num, Exp};
scale_up(Num, Den, Exp) ->
    scale_up(Num bsl 1, Den + 1, Exp - 1).

scale_down(Num, Den, Exp) ->
    NewDen = Den bsl 1,
    case Num < NewDen of
        true ->
            {Den bsr 52, Exp};
        false ->
            scale_down(Num, NewDen, Exp + 1)
    end.

decimal_to_float(Sign, Num, Den, Exp) ->
    Quo = Num div Den,
    Rem = Num - Quo * Den,

    Tmp =
        case Den bsr 1 of
            D when Rem > D ->
                Quo + 1;
            D when Rem < D ->
                Quo;
            _ when (Quo band 1) =:= 1 ->
                Quo + 1;
            _ ->
                Quo
        end,

    NewSign = case Sign of
                  -1 -> 1;
                  1 -> 0
              end,

    NewTmp = Tmp - ?POWER_OF_2_TO_52,

    NewExp = case (NewTmp < ?POWER_OF_2_TO_52) of
                 true ->  Exp;
                 false -> Exp + 1
             end,
    <<Float/float>> = <<NewSign:1, (NewExp + 1023):11, NewTmp:52>>,
    Float.

-define(NBASE, 10000).

decode_numeric_int(<<>>, Weight, Acc) -> {Acc, Weight};
decode_numeric_int(<<Digit:16/integer, Tail/binary>>, Weight, Acc) ->
    NewAcc = (Acc * ?NBASE) + Digit,
    decode_numeric_int(Tail, Weight - 1, NewAcc).

scale(Coef, 0) ->
    Coef;
scale(Coef, Diff) ->
    case Diff < 0 of
        true ->
            Coef div pg_math:pow10(-Diff);
        false ->
            Coef * pg_math:pow10(Diff)
    end.
