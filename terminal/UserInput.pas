unit UserInput;

interface
uses sysUtils;

function ReadString(prompt: String): String;
function ReadDouble(prompt: String): Double;
function ReadDoubleRange(prompt: String; min, max: Double): Double;
function ReadInteger(prompt: String): Integer;
function ReadIntegerRange(prompt: String; min, max: Integer): Integer;

implementation

	function ReadString(prompt: String): String;
	begin
		Write(prompt, ' ');
		ReadLn(result);
	end;

	function ReadDouble(prompt: String): Double;
	var
		userInput: String;
	begin
		userInput := ReadString(prompt);

		while not TryStrToFloat(userInput, result) do
		begin
			WriteLn('Please enter a number: ');
			userInput := ReadString(prompt);
		end;
	end;

	function ReadDoubleRange(prompt: String; min, max: Double): Double;
	begin
		result := ReadDouble(prompt);
		while (result < min) or (result > max) do
		begin
			WriteLn('Please enter a number between ', min, ' and ', max,'.');
			result := ReadDouble(prompt);
		end;
	end;

	function ReadInteger(prompt: String): Integer;
	var
		userInput: String;
	begin
		userInput := ReadString(prompt);

		while not TryStrToInt(userInput, result) do
		begin
			WriteLn('Please enter a whole number: ');
			userInput := ReadString(prompt);
		end;
	end;

	function ReadIntegerRange(prompt: String; min, max: Integer): Integer;
	begin
		result := ReadInteger(prompt);
		while (result < min) or (result > max) do
		begin
			WriteLn('Please enter a number between ', min, ' and ', max,'.');
			result := ReadInteger(prompt);
		end;
	end;
end.