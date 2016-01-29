-module(tokig_server).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/3]).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users = [],
                groups = [],
                memberships = []}).

start_link(Users, Groups, Memberships) ->
    lists:foreach(
        fun (Membership) ->
            case is_membership_valid(Users, Groups, Membership) of
                false -> erlang:error(invalid_membership);
                true  -> ok
            end
        end, Memberships),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-ifdef(TEST).
start_link_error_on_inexistent_user_test() ->
    ?assertError(invalid_membership, start_link([group], [], [{user, group}])),
    ?assertError(invalid_membership, start_link([group], [other_user], [{user, group}])).

start_link_error_on_inexistent_group_test() ->
    ?assertError(invalid_membership, start_link([], [user], [{user, group}])),
    ?assertError(invalid_membership, start_link([other_group], [user], [{user, group}])).

start_link_works_if_memberships_are_valid_test() ->
    {ok, Pid} = start_link([user], [group], [{user, group}]),
    ?assertNot(undefined == whereis(?MODULE)).
-endif.

init(Args) ->
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

is_membership_valid(Users, Groups, {User, Group}) ->
    lists:member(User, Users) andalso lists:member(Group, Groups).
