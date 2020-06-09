program BikeDrawing;
uses SplashKit;

// Three constant values are declared here, raidus, bikeHeight and bikeWidth
const
radius = 10;
bikeHeight = 30;
bikeWidth = 50;


// Create a new procedure called 'DrawBike'
// This procedure is going to take three parameters
// clr is the colour we will pass the procedure to draw the bike in
// x and y and the positioning points for the bike, this data will be passed to the procedure
procedure DrawBike(clr: Color; x, y: Integer);
var
// Declare variables for the position of the bikes wheels as Integers
wheel1X, wheel1Y, wheel2X, wheel2Y: Integer;
// Declare variables for the positioning of the frame as Integers
frameX1, frameY1, frameX2, frameY2, frameX3, frameY3: Integer;
// Declare variables for the positioning of the handle bars as 2D points on a cartesian plane
barX, barY: Point2D;

begin
	// This section deals with drawing the first wheel (back wheel)
	// Calculate the values for the variables, then assign those vales to their respective variable
	wheel1X := (x + radius);
	wheel1Y := y + (bikeHeight - radius);
	// Draw a circle with the passed colour at the points calculated and assigned to the variables wheel1X and wheel1Y
	// radius defines, you guessed it - the radius for the circle being drawn
	DrawCircle(clr, wheel1X, wheel1Y, radius);

	// This section deals with drawing the second wheel (front wheel)
	// Calculate the values for the variables, then assign those vales to their respective variable
	wheel2X := x + (bikeWidth - radius);
	wheel2Y := wheel1Y;
	// Draw a circle with the passed colour at the points calculated and assigned to the variables wheel2X and wheel2Y
	// radius defines, you guessed it - the radius for the circle being drawn
	DrawCircle(clr, wheel2X, wheel2Y, radius);

	// This section deals with drawing the bike frame
	// Calculate the values for the variables, then assign those vales to their respective variable
	frameX1 := wheel1X;
	frameY1 := wheel1Y;
	frameX2 := x + Round(bikeWidth / 2);
	frameY2 := y + Round(radius / 2);
	frameX3 := wheel2X;
	frameY3 := wheel2Y;
	// Draw a triangle with the passed colour at the points calculated and assigned to the variables
	// frameX1, frameY1, frameX2, frameY2, frameX3, frameY3
	DrawTriangle(clr, frameX1, frameY1, frameX2, frameY2, frameX3, frameY3);

	// This section deals with drawing the bike handle bars
	// Calculate the values for the variables, then assign those vales to their respective variable
	barX := PointAt(wheel2X, y);
	barY := PointAt(wheel2X, wheel2Y);
	// Draw a line with the passed colour from the points calculated and assigned to the variables barX, barY
	DrawLine(clr, barX, barY);
end;

	// This creates a new procedure called 'main', it has the program's
	// `main' instructions.
procedure Main();
begin
	// Call the OpenWindow procedure -> effect is a window appears
  	//  'Draw My Sweet Asssss Bike', title to set for the window
  	//  800, the width to set the window
 	//  200, the height to set the window
	OpenWindow('Draw My Sweet Asssss Bike', 800, 200);
	// Call the LoadDefaultColors procedure -> effect is SplashKit colors are initialised

	// Draw the screen white, so there is a clean canvis
	ClearScreen(ColorWhite);
	// Created a loop which will print 1000 bikes to the screen, all with random colour and positions.
	// If the user presses 'r', the loop will restart
	repeat
		ProcessEvents();
        DrawBike(RandomColor, Rnd(ScreenWidth()), Rnd(ScreenHeight()));
        RefreshScreen(60);
	until WindowCloseRequested('Draw My Sweet Asssss Bike');
end;

begin
	// Call procedure main
	Main();
end.
