program SillyName;

procedure SuperSillyName(name: String);
begin
	if (LowerCase(name) = 'reuben') then
		WriteLn(name, '! What an awesome name!')
	else
		Write(name, '?! What a terrible name!')
end;

procedure Main();
var
	name: String;
	option: String;

begin
	Write('What is your name? ');
	ReadLn(name);

	repeat
		WriteLn('Please select an option, ',name, '.');
		WriteLn(' - [S]illy name');
		WriteLn(' - [H]ello world');
		WriteLn(' - [Q]uit');
		WriteLn('Option -> ');
		ReadLn(option);

		case option of
			's', 'S': SuperSillyName(name);
			'h', 'H': WriteLn('Hello World!');
			'q', 'Q': WriteLn('Peace...');
		else 
			WriteLn('Woops ', name, '. Please try again.');
		end;
		WriteLn(); 
	until (LowerCase(option) = 'q');
end;

begin
	Main();
end.