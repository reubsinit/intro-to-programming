// Declare a program called 'RandomTriangles'
program RandomTriangles;
// 'RandomTriangles' uses 'SplashKit' and 'sgTypes' libraries
uses SplashKit;

// Create a new procedure called 'AhhTrianle'
// 'AhhTriangle' takes parameters as outlined
procedure AhhTriangle(clr: Color; x1, y1, x2, y2, x3, y3: Integer);
// 'AhhTriangle' is insrtucted to:
// Draw a trinagle in specified colour at specified points
begin
	DrawTriangle(clr, x1, y1, x2, y2, x3, y3);
end;

// Create a new procedure called 'Main'
procedure Main();
// Declare the variables to be used within 'Main'
var
	triangleColour: Color;
	triangleX1, triangleY1, triangleX2, triangleY2, triangleX3, triangleY3, drawCount: Integer;

// Open a window with the title of 'Triangles!' 800 wide and 600 heigh
// Some standard 'SplashKit' procedures are called for functionality:
// 'OpenAudio', 'LoadDefaultColors'
// Assign all the variables some default values
begin
	drawCount := 0;
	OpenAudio();
	OpenWindow('Triangles!', 800, 600);

	triangleColour := ColorGreen;
	triangleX1 := 0;
	triangleY1 := 0;
	triangleX2 := 0;
	triangleY2 := 0;
	triangleX3 := 0;
	triangleY3 := 0;

// create a repeat loop which calls upon a 'while' loop until the user closes the program
// So, while 'drawCount < 1000', draw 1000 triangles to the screen with random colour and sizes.
	repeat
		ProcessEvents();
    triangleX1 := Rnd(ScreenWidth());
    triangleY1 := Rnd(ScreenHeight());
    triangleX2 := Rnd(ScreenWidth());
    triangleY2 := Rnd(ScreenHeight());
    triangleX3 := Rnd(ScreenWidth());
    triangleY3 := Rnd(ScreenHeight());
    triangleColour := RandomColor;
    DrawTriangle(triangleColour, triangleX1, triangleY1, triangleX2, triangleY2, triangleX3, triangleY3);
    RefreshScreen(60);
	until WindowCloseRequested('Triangles!');


end;

begin
// Call 'Main'
	Main();
end.
