program HouseDrawing;
uses SplashKit;

procedure DrawBackground();
begin
	FillEllipse(ColorGreen, 0, 400, 800, 400);
end;

procedure DrawHouse();
begin
	FillRectangle(ColorGray, 300, 300, 200, 200);
	FillTriangle(ColorBlue, 250, 300, 400, 150, 550, 300);
	FillRectangle(ColorRed, 375, 425, 50, 75);
	FillRectangle(ColorYellow, 325, 350, 50, 50);
	FillRectangle(ColorYellow, 425, 350, 50, 50);
end;

procedure DrawNature();
begin
	FillEllipse(ColorRed, 50, 50, 150, 150);
end;

procedure Main();
begin
	OpenWindow('House Drawing', 800, 600);

	ClearScreen(ColorWhite);
	DrawBackground();
	DrawHouse();
	DrawNature();
	RefreshScreen(60);
	Delay(6000);
end;

begin
	Main();
end.
