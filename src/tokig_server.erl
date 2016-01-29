-module(tokig_server).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/3,
         add_user/2,
         remove_user/2,
         add_group/2,
         remove_group/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {users = [],
                groups = [],
                memberships = []}).

%% API

start_link(Users, Groups, Memberships) ->
    lists:foreach(
        fun (Membership) ->
            case is_membership_valid(Users, Groups, Membership) of
                false -> erlang:error(invalid_membership);
                true  -> ok
            end
        end, Memberships),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Users, Groups, Memberships], []).

-ifdef(TEST).
start_link_error_on_inexistent_user_test() ->
    ?assertError(invalid_membership, start_link([group], [], [{user, group}])),
    ?assertError(invalid_membership, start_link([group], [other_user], [{user, group}])).

start_link_error_on_inexistent_group_test() ->
    ?assertError(invalid_membership, start_link([], [user], [{user, group}])),
    ?assertError(invalid_membership, start_link([other_group], [user], [{user, group}])).

start_link_works_if_memberships_are_valid_test_() ->
    server_test([user], [group], [{user, group}], fun (Pid) -> [
        ?_assert(Pid == whereis(?MODULE))
    ] end).
-endif.

add_user(Pid, User) ->
    gen_server:call(Pid, {add_user, User}).

-ifdef(TEST).
add_user_test_() ->
    server_test([], [], [], fun (Pid) -> [
        ?_assertMatch(ok, add_user(Pid, user)),
        ?_assertMatch(#state{users = [user]}, gen_server:call(Pid, get_state))
    ] end).
-endif.

remove_user(Pid, User) ->
    gen_server:call(Pid, {remove_user, User}).

-ifdef(TEST).
remove_user_test_() ->
    server_test([user], [], [], fun (Pid) -> [
        ?_assertMatch(ok, remove_user(Pid, user)),
        ?_assertMatch(#state{users = []}, gen_server:call(Pid, get_state))
    ] end).
-endif.

add_group(Pid, Group) ->
    gen_server:call(Pid, {add_group, Group}).

-ifdef(TEST).
add_group_test_() ->
    server_test([], [], [], fun (Pid) -> [
        ?_assertMatch(ok, add_group(Pid, group)),
        ?_assertMatch(#state{groups = [group]}, gen_server:call(Pid, get_state))
    ] end).
-endif.

remove_group(Pid, Group) ->
    gen_server:call(Pid, {remove_group, Group}).

-ifdef(TEST).
remove_group_test_() ->
    server_test([], [group], [], fun (Pid) -> [
        ?_assertMatch(ok, remove_group(Pid, group)),
        ?_assertMatch(#state{groups = []}, gen_server:call(Pid, get_state))
    ] end).
-endif.

%% pure functions

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

do_add_group(Group, Groups) ->
    [Group|Groups].

-ifdef(TEST).
do_add_group_test() ->
    NewGroups = do_add_group(new, [old, some_group]),
    ?assert(lists:member(new, NewGroups)).
-endif.

do_remove_group(Group, Groups) ->
    lists:delete(Group, Groups).

-ifdef(TEST).
do_remove_group_test() ->
    NewGroups = do_remove_group(old, [old, some_group]),
    ?assertNot(lists:member(old, NewGroups)).
-endif.

%% gen_server callbacks

init([Users, Groups, Memberships]) ->
    {ok, #state{users = Users, groups = Groups, memberships = Memberships}}.

handle_call({add_user, User}, _From, #state{users = Users} = State) ->
    {reply, ok, State#state{users = do_add_user(User, Users)}};
handle_call({remove_user, User}, _From, #state{users = Users} = State) ->
    {reply, ok, State#state{users = do_remove_user(User, Users)}};
handle_call({add_group, Group}, _From, #state{groups = Groups} = State) ->
    {reply, ok, State#state{groups = do_add_group(Group, Groups)}};
handle_call({remove_group, Group}, _From, #state{groups = Groups} = State) ->
    {reply, ok, State#state{groups = do_remove_group(Group, Groups)}};
handle_call(get_state, _From, State) ->
    {reply, State, State};
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

% test helpers

-ifdef(TEST).
server_test(Users, Groups, Memberships, Test) ->
    {setup,
        fun () -> {ok, Pid} = start_link(Users, Groups, Memberships), Pid end,
        fun (Pid) -> gen_server:stop(Pid) end,
        Test
    }.
-endif.
