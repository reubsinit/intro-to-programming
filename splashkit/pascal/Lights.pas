program	Lights;
uses SplashKit;

procedure LoadResources();
begin
	LoadBitmap('light on', 'on_sml.png');
	LoadBitmap('light off', 'off_sml.png');
end;

procedure DrawLight(isOn: Boolean; x, y: Integer);
begin
	if isOn then
		DrawBitmap(BitmapNamed('light on'), x, y)
	else
		DrawBitmap(BitmapNamed('light off'), x, y);
end;

procedure Main();
var
	myLightIsOn: Boolean;
	lightX: Integer;
begin
  OpenAudio();

  OpenWindow('Lights!', 800, 600);

  LoadResources();

  myLightIsOn := False;
  lightX := Round(ScreenWidth() / 2);

  repeat
  	// WriteLn('here');
  	ProcessEvents();

  	ClearScreen(ColorWhite);

  	if KeyTyped(SPACE_KEY) then
  		myLightIsOn := not myLightIsOn;
  	if KeyDown(LEFT_KEY) then
  		lightX -= 3;
  	if KeyDown(RIGHT_KEY) then
  		lightX += 3;

  	if lightX < -BitmapWidth(BitmapNamed('light on')) then
  		lightX := ScreenWidth();

    if lightX > ScreenWidth() then
      lightX := -BitmapWidth(BitmapNamed('light on'));

  	DrawLight(myLightIsOn, lightX, 100);

  	RefreshScreen(60);
  until WindowCloseRequested('Lights!') ;
end;

begin
	Main();
end.
