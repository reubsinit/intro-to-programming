program Stats;
uses SysUtils, UserInput;

procedure PopulateArray(var data: array of Double);
var
	i: Integer;
begin
	for i := Low(data) to High(data) do
	begin
		WriteLn('For index ', i + 1, '...');
		data[i] := ReadDouble('Please enter a value: ');
	end;
end;

procedure Swap(var v1, v2: Double);
var
	temp: Double;
begin
	temp := v2;
	v2 := v1;
	v1 := temp;	
end;

procedure Sort(var data: array of Double);
var
	i, j: Integer;
begin
	for i := High(data) downto Low(data) do
	begin
		for j := Low(data) to (i - 1) do
		begin
			if data[j] > data[j + 1] then
			begin
				swap(data[j], data[j + 1]);
			end;
		end;
	end;
	
end;

procedure PrintArray(const numbers: array of Double);
var
	i: Integer;
begin
	WriteLn('You entered');
	for i := Low(numbers) to High(numbers) do
	begin
		WriteLn('- ', numbers[i]:4:2);
	end;
end;

function Sum(const numbers: array of Double): Double;
var
	i: Integer;
begin
	result := 0;
	for i := Low(numbers) to High(numbers) do
	begin
		result := numbers[i] + result;
	end;
end;

function Mean(const numbers: array of Double): Double;
begin
	result := Sum(numbers) / Length(numbers);
end;

function Max(const numbers: array of Double): Double;
var
	i: Integer;
begin
	result := numbers[0];
	for i := Low(numbers) to High(numbers) do
	begin
		if result < numbers[i] then
			result := numbers[i];
	end;	
end;

function Median(const numbers: array of Double): Double;
var
	temp: Integer;
begin
	temp := Length(numbers);
	result := temp mod 2;
		if result = 0 then
		begin
			temp := Round(temp / 2 - 1);
			result := Round(numbers[temp] + numbers[temp + 1]) / 2;
		end
		else
			temp := Round(((temp - 1) / 2));
			result := numbers[temp];
end;

procedure Main();
var
	numbers: array of Double;
	userLength: Integer;
begin
	userLength := ReadInteger('How long do you wish to set your array: ');
	SetLength(numbers, userLength);
	PopulateArray(numbers);
	sort(numbers);
	PrintArray(numbers);
	WriteLn('Sum is ', Sum(numbers):4:2);
	WriteLn('Mean is ', Mean(numbers):4:2);
	WriteLn('Max is ', Max(numbers):4:2);
	WriteLn('Median is ', Median(numbers):4:2);
end;

begin
	Main();
end.