-module(records).
-compile(export_all).


-record(robot, {
			name,
			type=industrial,
			hobbies,
			details=[]}).

first_robot() ->
		#robot{
				name="srini",
				hobbies="Sitting"				
		}.
