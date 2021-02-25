-module(records).
-compile(export_all).
-include("records.hrl").

-record(customer, {name = "", bal = 0.00}).
-record(robot, {name, type=industrial, hobbies, details=[]}). 
-record(user, {id, name, group, age}).

	
record_stuff() ->
	% Define a customer
	My_customer = #customer{name="John Smith", bal=100.00},
	
	% Change data
	My_customer_2 = My_customer#customer{bal = 50},
	
	% Output data
	io:fwrite("~p owes $ ~p\n", [My_customer_2#customer.name, My_customer_2#customer.bal]).



first_robot() ->
  #robot{name="Mechatron", type=handmade,  details=["Moved by a small man inside"]}.

car_factory(CorpName) ->
  #robot{name=CorpName, hobbies="building cars"}.




Crusher = #robot{name="Crusher", hobbies=["Crushing people","petting cats"]}.
Crusher#robot.hobbies.
% ["Crushing people","petting cats"]

NestedBot = #robot{details=#robot{name="erNest"}}.
% #robot{name = undefined, type = industrial, hobbies = undefined, details = #robot{name = "erNest",type = industrial, hobbies = undefined,details = []}}
(NestedBot#robot.details)#robot.name.
% "erNest"



%% use pattern matching to filter
admin_panel(#user{name=Name, group=admin}) ->
  Name ++ " is allowed!";

admin_panel(#user{name=Name}) ->
  Name ++ " is not allowed".


%% can extend user without problem
adult_section(U = #user{}) when U#user.age >= 18 ->
  %% Show stuff that can't be written in such a text
  allowed;

adult_section(_) ->
  %% redirect to sesame street site
  forbidden.


repairman(Rob) ->
  Details = Rob#robot.details,
  NewRob = Rob#robot{details=["Repaired by repairman"|Details]}, {repaired, NewRob}.



included() -> #included{some_field="Some value"}.

% I strongly recommend that you keep all record definitions local, within one module.
% If you want some other module to look at a record's innards, write functions to access its fields and keep its details as private as possible. 



% The Erlang shell has a command rr(Module) that lets you load record definitions from Module