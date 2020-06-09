program Mandelbrot;
uses SplashKit;

const
	MAX_ITERATION = 1000;

function IterationColour(iteration: Integer): Color;
var
hue: Double;

begin
	if (iteration >= MAX_ITERATION) then
	begin
		result := ColorBlack;
	end
	else
	begin
		hue := (0.5 + (iteration / MAX_ITERATION));
		if (hue > 1) then
		begin
			hue -= hue;
		end
		else
		begin
			result := HSBColor(hue, 0.8, 0.9);
		end;
	end;

end;

function MandlebrotColour(mbX, mbY: Double): Color;
var
xTemp, x, y: Double;
iteration: Integer;

begin
	x := mbX;
	y := mbY;
	iteration := 0;
	while (((x * x) + (y * y)) <= 4) and (iteration < MAX_ITERATION) do
		begin
			xtemp := ((x * x) - (y * y) + mbX);
			y := (2 * x * y + mbY);
			x := xTemp;
			iteration := (iteration + 1);
		end;
	result := IterationColour(iteration);

end;

procedure DrawMandlebrot(startMbX, startMbY, mbWidth, mbHeight: Double);
var
scaleWidth, scaleHeight, mX, mY: Double;
x, y: Integer;
mbColour: Color;

begin
	scaleWidth := (mbWidth / ScreenWidth());
	scaleHeight := (mbHeight / ScreenHeight());
	x := 0;
	while (x < ScreenWidth()) do
	begin
		y := 0;
		while (y < ScreenHeight()) do
		begin
			mx := (startMbX + (x * scaleWidth));
			my := (startMbY + (y * scaleHeight));
			mbColour :=  MandlebrotColour(mx, my);
			DrawPixel(mbColour, x, y);
			y := y +1;
		end;
		x := x +1;
	end;
end;

procedure Main();
var
startMbX, startMbY, mbWidth, mbHeight, zoomWidth, zoomHeight: Double;

begin
	startMbX := -2.5;
	startMbY := -1.5;
	mbWidth := 4;
	mbHeight := 3;
	zoomWidth := (mbWidth / 2);
	zoomHeight := (mbHeight / 2);
	OpenWindow('MandelWhat?', 800, 600);


	repeat
		ProcessEvents();
		if MouseClicked(LEFT_BUTTON) then
		begin
			mbHeight := (mbHeight / 2);
			mbWidth := (mbWidth / 2);
			startMbX := (startMbX + ((MouseX() * mbWidth) / ScreenWidth()));
			startMbY := (startMbY + ((MouseY() * mbHeight) / ScreenHeight()));
		end;
		if MouseClicked(RIGHT_BUTTON) then
		begin
			mbHeight := (mbHeight * 2);
			mbWidth := (mbWidth * 2);
			startMbX := (startMbX - (startMbX + ((MouseX() * mbWidth) / ScreenWidth())));
			startMbY := (startMbY - (startMbY + ((MouseY() * mbHeight) / ScreenHeight())));
		end;
		DrawMandlebrot(startMbX, startMbY, mbWidth, mbHeight);
		RefreshScreen(60);
	until WindowCloseRequested('MandelWhat?');

end;

begin
	Main();
end.

