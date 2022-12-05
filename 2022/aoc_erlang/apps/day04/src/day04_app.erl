-module(day04_app).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% 2-4,6-8
%% 2-3,4-5
%% 5-7,7-9
%% 2-8,3-7
%% 6-6,4-6
%% 2-6,4-8
%%
%% [{{2, 4}, {6, 8}}, {{2, 3}, {4, 5}}, {{5, 7}, {7, 9}}, {{2, 8}, {3, 7}}, {{6, 6}, {4, 6}}, {{2, 6}, {4, 8}}]

contains({L1, U1}, {L2, U2}) when L1 >= L2, U1 =< U2 -> true;
contains({L1, U1}, {L2, U2}) when L2 >= L1, U2 =< U1 -> true;
contains(_R1, _R2) -> false.

filter_overlapped(List) ->
    %%[{{2, 8}, {3, 7}}, {{6, 6}, {4, 6}}].
    not_implemented.

contains_test() ->
    ?assertEqual(false, contains({2, 4}, {6, 8})),
    ?assertEqual(true, contains({2, 8}, {3, 7})),
    ?assertEqual(false, contains({5, 7}, {7, 9})).

filter_overlapped_test() ->
    Input =  [{{2, 4}, {6, 8}}, {{2, 3}, {4, 5}}, {{5, 7}, {7, 9}}, {{2, 8}, {3, 7}}, {{6, 6}, {4, 6}}, {{2, 6}, {4, 8}}],
    ?assertEqual([{{2, 8}, {3, 7}}, {{6, 6}, {4, 6}}], filter_overlapped(Input)).

-endif.
