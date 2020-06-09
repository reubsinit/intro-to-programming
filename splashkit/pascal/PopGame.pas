// Declare the name: PopGame
program PopGame;

// Declare used libraries
uses SplashKit, sysutils;

// Constant values that PopGame uses
const
	MIN_CIRCLE_WIDTH = 50;
	MAX_CIRCLE_WIDTH = 100;
	NUM_SHAPES = 10;

// Enumeration ShapKind is decaled to off a range of options
// In relation to the shapes drawn in game
// Custom datetypes Shape and PopGameData are declared to handle
// All the required data that the game will need to run
type
	ShapeKind = (CircleKind, RectangleKind, TriangleKind);

	Shape = record
		colour: Color;
		visible: Boolean;
		case kind: ShapeKind of
			CircleKind: (circleShape: Circle;);
			RectangleKind: (rectangleShape: Rectangle;);
			TriangleKind: (triangleShape: Triangle;);
	end;

	PopGameData = record
		shapes: array [0..NUM_SHAPES - 1] of Shape;
		score: Integer;
		shapesRemaining: Integer;
		popTimer: Timer;
	end;

// LoadResources is used to load a font used to draw
// The user's score and timer
procedure LoadResources();
begin
	LoadFont('game font', 'arial.ttf');
end;

// DrawScore is used to draw the user's current score to the screen
procedure DrawScore(var data: PopGameData);
var
	score: String;
begin
	score := IntToStr(data.score);
	DrawText(score, ColorRed, ScreenWidth() - 80, 0);
end;

// DrawShotClock draws the timer which represents the time elapsed between shape pops
// To the screen
procedure DrawShotClock(data: PopGameData);
var
	shotTimer: String;
	shotTimerLocation: Point2D;
begin
	shotTimerLocation.x := 10;
	shotTimerLocation.y := 5;
	shotTimer := IntToStr(TimerTicks(TimerNamed('Pop Timer')));
	if TimerTicks(TimerNamed('Pop Timer')) < 700 then
		DrawText(shotTimer, ColorGreen, shotTimerLocation.x, shotTimerLocation.y)
	else if (TimerTicks(TimerNamed('Pop Timer')) > 700) and
	(TimerTicks(TimerNamed('Pop Timer')) < 2000) then
		DrawText(shotTimer, ColorBlue, shotTimerLocation.x, shotTimerLocation.y)
	else
		DrawText(shotTimer, ColorRed, shotTimerLocation.x, shotTimerLocation.y);
end;

// ShapeAtPoint is used to determine whether there is a shape
// At a variable point on the screen
// That point represents the mouse loction
function ShapeAtPoint(const data: Shape; mousepoint: Point2D): Boolean;
var
	i: Integer;
begin
	result := False;
	case data.kind of
		CircleKind: result := PointInCircle(mousepoint, data.circleShape);
		RectangleKind: result := PointInRectangle(mousepoint, data.rectangleShape);
		TriangleKind: result := PointInTriangle(mousepoint, data.triangleShape);
	end;
end;

// HandleInput is used to process all the user's in game input
// When A user clicks the screen, that point is then checked
// Using ShapeAtPoint, if there is shape, it POPS!
procedure HandleInput(var data: PopGameData);
var
	i: Integer;
begin
	for i := Low(data.shapes) to High(data.shapes) do
	begin
		if ShapeAtPoint(data.shapes[i], MousePosition()) and
		MouseClicked(LEFT_BUTTON) and
		data.shapes[i].visible then
		begin
			data.shapes[i].visible := False;
			data.score += 10;
			data.shapesRemaining -= 1;
			ResetTimer(TimerNamed('Pop Timer'));
			break;
		end;
	end;
end;

// PlaceShape is used to randomly position random shapes on the screen
// For the user to POP!
// Rectangle, Triangle, Circle!
procedure PlaceShape(var shapeToPlace: Shape; center: Point2D);
var
	radius: Integer;
begin
	radius := Round(MIN_CIRCLE_WIDTH + (MAX_CIRCLE_WIDTH - MIN_CIRCLE_WIDTH) * Rnd());
	shapeToPlace.visible := True;
	shapeToPlace.kind := ShapeKind(Rnd(3));
	case shapeToPlace.kind of
		CircleKind:
		begin
			shapeToPlace.circleShape := CircleAt(center, radius);
		end;
		RectangleKind:
		begin
			shapeToPlace.rectangleShape := RectangleFrom(center, radius, radius);
		end;
		TriangleKind:
		begin
			shapeToPlace.triangleShape := TriangleFrom(center.x, center.y - radius, center.x - (radius/2), center.y, center.x + (radius / 2), center.y);
		end;
	end;
end;

// CreateShapeAt calls upon PlaceShape to generate shape locations
function CreateShapeAt(center: Point2D; colour: Color): Shape;
begin
	result.colour := colour;
	PlaceShape(result, center);
end;

// DrawShape is used to fill the shapes with color. Making them visible to the user
procedure DrawShape(var shapeToDraw: Shape);
begin
	if shapeToDraw.visible then
	begin
		case shapeToDraw.kind of
		CircleKind:
			FillCircle(shapeToDraw.colour, shapeToDraw.circleShape);
		RectangleKind:
			FillRectangle(shapeToDraw.colour, shapeToDraw.rectangleShape);
		TriangleKind:
			FillTriangle(shapeToDraw.colour, shapeToDraw.triangleShape);
		end;
	end;
end;

// InitGame is used to set all the default startup data for the game
// It creates the first wave of shapes, sets the sore to zero
procedure InitGame(var data: PopGameData);
var
	i: Integer;
begin
	LoadResources();
	for i := Low(data.shapes) to High(data.shapes) do
	begin
		data.shapes[i] := CreateShapeAt(RandomScreenPoint(), RandomRGBColor(200));
		data.shapes[i].visible := true;
	end;
	data.shapesRemaining := NUM_SHAPES;
	data.score := 0;
	data.popTimer := CreateTimer('Pop Timer');
	StartTimer(TimerNamed('Pop Timer'));
end;

// StartNewWave is used to draw new shapes to the screen once the user has popped all
// Of the shapes in the current wave
procedure StartNewWave(var data: PopGameData);
var
	i: Integer;
begin
	data.shapesRemaining := NUM_SHAPES;
	for i := Low(data.shapes) to High(data.shapes) do
	begin
		PlaceShape(data.shapes[i], RandomScreenPoint());
	end;
end;

// DrawGame draws all ingame elements to the screen.
// Shapes, Score and Timer
procedure DrawGame(var data: PopGameData);
var
	i: Integer;
begin
	for i := Low(data.shapes) to High(data.shapes) do
	begin
		DrawShape(data.shapes[i]);
	end;
	DrawScore(data);
	DrawShotClock(data);
end;

// UpdateGame is used to check if a new wave of shapes needs to be spawned
procedure UpdateGame(var data: PopGameData);
begin
	if data.shapesRemaining = 0 then
		StartNewWave(data);
end;

// Main
procedure Main();
var

	data: PopGameData;
begin
	OpenWindow('Pop Game', 800, 600);

	InitGame(data);
	repeat
		ProcessEvents();
		ClearScreen(ColorWhite);
		HandleInput(data);
		UpdateGame(data);
		DrawGame(data);
		RefreshScreen(60);
	until WindowCloseRequested('Pop Game');

end;

begin
	Main();
end.


