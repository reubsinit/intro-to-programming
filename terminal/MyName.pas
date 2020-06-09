// Declare our program, in this case the name of the program is "MyName".
program MyName;

// Create a new procedure called 'MyNameIs'.
// Declare a variable called 'name'. This variable will accept data as a string.
// Print the text 'What is your name broheme? ' to the screen!
// The procedure 'ReadLn' is used to accept the users input (as a string)
// and assigns the input to the variable 'name'.
// Then a combination of text and the variables ('name') value is printed to the screen.
procedure MyNameIs;
var name: string;

begin
Write('What is your name broheme? ');
	ReadLn(name);
	WriteLn('So, your name is ', name, '!');
end;

// Create a new procedure called 'Main'.
// Called the procedure 'MyNameIs'.
procedure Main();
begin
	MyNameIs();
end;

// Call the procedure 'Main'.
begin
	Main();
end.