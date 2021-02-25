preschool() ->
	'Go to preschool'.
 
kindergarten() ->
	'Go to kindergarten'.
 
grade_school() ->
	'Go to grade school'.
 
what_grade(X) ->
	if 
    X < 5 -> preschool();
    X == 5 -> kindergarten();
    X > 5 -> grade_school()
	end.