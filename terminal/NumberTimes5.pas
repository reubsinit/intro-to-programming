program NumberTimes5;
// Create a new function called TimesByFive.
// This function will be passed a parameter 'val' which is an integer.
function TimesByFive(val : Integer): Integer;

// The functions is going to provide us with a value or 'result'
// which represents the product of 'val' and 5
// E.g. If 'val' is assigned the value 6, then the function will return the product of 6 and 5
// which is 30!
begin
	result := val * 5;
end;

// Create a procedure called 'Main'
procedure Main();

// Declare two variables as integers for use within this procedure
// The variables are 'number' and 'numberTimesFive'
var
	number : Integer;
	numberTimesFive  : Integer;

// Print to the screen 'What number would you like to multiply by 5? '
// ReadLn is called to store the user's input to the variable 'number', input must be an integer as declared above
// 'numberTimesFive' then has it's value calculated and stored in it
// So, in this case, the integer assigned to 'number' is passed as a parameter to the function 'TimesByFive'
// The value returned by that function is assigned to 'numberTimesFive'
// Then, we print a lovely little message to the screen with the value of 'numberTimesFive'
begin
	Write('What number would you like to multiply by 5? ');
	ReadLn(number);
	numberTimesFive := TimesByFive(number);
	WriteLn('That number times by 5 is ', numberTimesFive);
end;

begin
	Main();
end.