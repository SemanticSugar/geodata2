-module(geodata2_ip).

%% API
-export([make_ip/1]).

-spec make_ip(MaybeIP) -> {ok, binary(), IPVersion} | {error, format}
    when IPVersion :: 4 | 6,
         MaybeIP ::
             {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()} |
             non_neg_integer() |
             {non_neg_integer(),
              non_neg_integer(),
              non_neg_integer(),
              non_neg_integer(),
              non_neg_integer(),
              non_neg_integer(),
              non_neg_integer(),
              non_neg_integer()} |
             binary() |
             string().
make_ip({B3, B2, B1, B0})
    when is_integer(B0), is_integer(B1), is_integer(B2), is_integer(B3),
         B0 >= 0 andalso B0 =< 255, B1 >= 0 andalso B1 =< 255, B2 >= 0 andalso B2 =< 255,
         B3 >= 0 andalso B3 =< 255 ->
    {ok, <<B3:8, B2:8, B1:8, B0:8>>, 4};
make_ip(IP) when is_integer(IP), IP =< 16#FFFFFFFF ->
    {ok, <<IP:32>>, 4};
make_ip({W7, W6, W5, W4, W3, W2, W1, W0})
    when is_integer(W0), is_integer(W1), is_integer(W2), is_integer(W3), is_integer(W4),
         is_integer(W5), is_integer(W6), is_integer(W7), W0 >= 0 andalso W0 =< 65535,
         W1 >= 0 andalso W1 =< 65535, W2 >= 0 andalso W2 =< 65535, W3 >= 0 andalso W3 =< 65535,
         W4 >= 0 andalso W4 =< 65535, W5 >= 0 andalso W5 =< 65535, W6 >= 0 andalso W6 =< 65535,
         W7 >= 0 andalso W7 =< 65535 ->
    {ok, <<W7:16, W6:16, W5:16, W4:16, W3:16, W2:16, W1:16, W0:16>>, 6};
make_ip(IP) when is_integer(IP) ->
    {ok, <<IP:128/integer>>, 6};
make_ip(IP) when is_binary(IP) ->
    make_ip(binary_to_list(IP));
make_ip(IP) when is_list(IP) ->
    try address_fast(IP, 0, 24) of
        N ->
            make_ip(N)
    catch
        _:_ ->
            case inet_parse:address(IP) of
                {ok, Tuple} ->
                    make_ip(Tuple);
                Error ->
                    Error
            end
    end;
make_ip(_) ->
    {error, format}.

address_fast([N2, N1, N0, $. | Rest], Num, Shift)
    when Shift >= 8, "000" =< [N2, N1, N0], [N2, N1, N0] =< "255" ->
    address_fast(Rest, Num bor (list_to_integer([N2, N1, N0]) bsl Shift), Shift - 8);
address_fast([N1, N0, $. | Rest], Num, Shift)
    when Shift >= 8, "00" =< [N1, N0], [N1, N0] =< "99" ->
    address_fast(Rest, Num bor (list_to_integer([N1, N0]) bsl Shift), Shift - 8);
address_fast([N0, $. | Rest], Num, Shift) when Shift >= 8, $0 =< N0, N0 =< $9 ->
    address_fast(Rest, Num bor (N0 - $0 bsl Shift), Shift - 8);
address_fast([_N2, _N1, _N0] = L, Num, 0) when "000" =< L, L =< "255" ->
    Num bor list_to_integer(L);
address_fast([_N1, _N0] = L, Num, 0) when "00" =< L, L =< "99" ->
    Num bor list_to_integer(L);
address_fast([N0], Num, 0) when $0 =< N0, N0 =< $9 ->
    Num bor (N0 - $0).
