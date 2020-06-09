// Declare program: SortVisualiser
program SortVisualiser;
// Declare libraries used by program
uses SplashKit, sysutils;

procedure PopulateArray(var numbers: array of Integer);
var
	i: Integer;
begin
	for i := Low(numbers) to High(numbers) do
	begin
		numbers[i] := Random(ScreenHeight());
	end;

end;

procedure PlotBars(data: array of Integer);
var
	i: Integer;
	barWidth: Integer;
	totalBars: Integer;
	barX, barY: Integer;
begin
	barX := 0;
	totalBars := Length(data);
	barWidth := Round(ScreenWidth() / totalBars);
	for i:= Low(data) to High(data) do
	begin
		barY := ScreenHeight - data[i];
		FillRectangle(ColorRed, barX, barY, barWidth, data[i]);
		barX += barWidth;
	end;

end;

procedure PrintArray(const numbers: array of Integer);
var
	i: Integer;
begin
	for i := Low(numbers) to High(numbers) do
	begin
		WriteLn('You entered ', numbers[i]);
	end;
end;

procedure Swap(var v1, v2: Integer);
var
	temp: Double;
begin
	temp := Round(v2);
	v2 := Round(v1);
	v1 := Round(temp);

end;

procedure Sort(var data: array of Integer);
var
	i, j: Integer;
begin
	for i := High(data) downto Low(data) do
	begin
		for j := Low(data) to (i - 1) do
		begin
			if data[j] > data[j + 1] then
			begin
				Swap(data[j], data[j + 1]);
				ClearScreen(ColorWhite);
				PlotBars(data);
				RefreshScreen(60);
			end;
		end;
	end;
end;

procedure DoSort(var data: array of Integer);
begin
	ClearScreen(ColorWhite);
	Sort(data);

end;

procedure Main();
var
	default: array [0..25] of Integer;
begin
	OpenWindow('Sortsss', 800, 600);
	PopulateArray(default);
	ClearScreen(ColorWhite);
	repeat
		ProcessEvents();
		PlotBars(default);
		RefreshScreen(60);
		if KeyDown(S_KEY) then
		begin
			DoSort(default);
		end;
		if KeyDown(R_KEY) then
		begin
			ClearScreen(ColorWhite);
			PopulateArray(default);
			PlotBars(default);
			RefreshScreen(60);
		end;
	until WindowCloseRequested('Sortsss');
end;

begin
	Main();
end.
