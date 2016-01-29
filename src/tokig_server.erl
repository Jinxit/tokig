-module(tokig_server).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/3,
         add_user/2,
         remove_user/2]).

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

add_user(Pid, User) ->
    gen_server:cast(Pid, {add_user, User}).

remove_user(Pid, User) ->
    gen_server:cast(Pid, {remove_user, User}).

init(Args) ->
    {ok, undefined}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({add_user, User}, #state{users = Users} = State) ->
    {noreply, State#state{users = do_add_user(User, Users)}};
handle_cast({remove_user, User}, #state{users = Users} = State) ->
    {noreply, State#state{users = do_remove_user(User, Users)}};
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

do_add_user(User, Users) ->
    [User|Users].

-ifdef(TEST).
do_add_user_test() ->
    NewUsers = do_add_user(new, [old, someone]),
    ?assert(lists:member(new, NewUsers)).
-endif.

do_remove_user(User, Users) ->
    lists:delete(User, Users).

-ifdef(TEST).
do_remove_user_test() ->
    NewUsers = do_remove_user(old, [old, someone]),
    ?assertNot(lists:member(old, NewUsers)).
-endif.
