-module(geodata2_ip).

-include("geodata2.hrl").

%% API
-export([make_ip/1]).

make_ip({B3, B2, B1, B0})
    when is_integer(B0),
         is_integer(B1),
         is_integer(B2),
         is_integer(B3),
         B0 >= 0 andalso B0 =< 255,
         B1 >= 0 andalso B1 =< 255,
         B2 >= 0 andalso B2 =< 255,
         B3 >= 0 andalso B3 =< 255 ->
    {ok, <<B3:8, B2:8, B1:8, B0:8>>, ?IPV4};
make_ip(IP) when is_integer(IP), IP =< 16#FFFFFFFF ->
    {ok, <<IP:32>>, ?IPV4};
make_ip({W7, W6, W5, W4, W3, W2, W1, W0})
    when is_integer(W0),
         is_integer(W1),
         is_integer(W2),
         is_integer(W3),
         is_integer(W4),
         is_integer(W5),
         is_integer(W6),
         is_integer(W7),
         W0 >= 0 andalso W0 =< 65535,
         W1 >= 0 andalso W1 =< 65535,
         W2 >= 0 andalso W2 =< 65535,
         W3 >= 0 andalso W3 =< 65535,
         W4 >= 0 andalso W4 =< 65535,
         W5 >= 0 andalso W5 =< 65535,
         W6 >= 0 andalso W6 =< 65535,
         W7 >= 0 andalso W7 =< 65535 ->
    {ok, <<W7:16, W6:16, W5:16, W4:16, W3:16, W2:16, W1:16, W0:16>>, ?IPV6};
make_ip(IP) when is_integer(IP) ->
    {ok, <<IP:128/integer>>, ?IPV6};
make_ip(IP) when is_binary(IP) ->
    make_ip(binary_to_list(IP));
make_ip(IP) when is_list(IP) ->
    case catch address_fast(IP, 0, 24) of
        N when is_integer(N) ->
            make_ip(N);
        _ ->
            case inet_parse:address(IP) of
                {ok, Tuple} ->
                    make_ip(Tuple);
                Error ->
                    Error
            end
    end;
make_ip(_) ->
    {error, format}.

address_fast([N2, N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N2, N1, N0]) of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N1, N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case list_to_integer([N1, N0]) of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast([N0, $. | Rest], Num, Shift) when Shift >= 8 ->
    case N0 - $0 of
        N when N =< 255 ->
            address_fast(Rest, Num bor (N bsl Shift), Shift - 8)
    end;
address_fast(L = [_N2, _N1, _N0], Num, 0) ->
    case list_to_integer(L) of
        N when N =< 255 ->
            Num bor N
    end;
address_fast(L = [_N1, _N0], Num, 0) ->
    case list_to_integer(L) of
        N when N =< 255 ->
            Num bor N
    end;
address_fast([N0], Num, 0) ->
    case N0 - $0 of
        N when N =< 255 ->
            Num bor N
    end.
