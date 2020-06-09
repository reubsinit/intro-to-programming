unit TrexLib;

interface
uses SplashKit, sysutils;

// Constant values used by different elements in the game
const
  PLAYER_SPEED = 2;
  ORB_SPEED = 4;
  ENEMY_SPEED = 1;
  MAX_ORBS = 10;
  MAX_LIVES = 3;

// OrbData:
// Used to store any data associated with the path of the bullets in game
type
  OrbData = record
    xDistance:    Double;
    yDistance:    Double;
    hypot:        Double;
    moveRatio:    Double;
    count:        Integer;
  end;

  // Orb:
  // Used to store any data associated with the bullets in game
  // i.e. position, sprite
  Orb = record
    sprite:       Sprite;
    xPos:         Double;
    yPos:         Double;
    data:         OrbData;
    visible:      Boolean;
  end;

  // MovementControl:
  // Used to store any data which is required to calculate player/arena collisions
  MovementControl = record
    playerPoint:    Point2D;
    boundaryPoint:  Point2D;
  end;

  // Player:
  // Used to store any data associated with the player in game
  // i.e. position, sprite, score, health
  Player = record
    sprite:   Sprite;
    xPos:     Double;
    yPos:     Double;
    angle:    Integer;
    score:    Integer;
    health:	  Integer;
    bullet:   array [0..MAX_ORBS] of Orb;
    geometry: MovementControl;
  end;

  // Enemy:
  // Used to store any data associated with the enemies in game
  // i.e. sprite, position
  Enemy = record
    sprite:         Sprite;
    xPos:           Double;
    yPos:           Double;
    visible:        Boolean;
  end;

  // GameData:
  // Is used to track all values in game. Consists of:
  // Enemy Data
  // Player Data
  // Spawn locations etc
  GameData = record
    sessionPlayer:      Player;
    sessionEnemies:     array of Enemy;
    displayCoordsCount: Integer;
    locationKey:        Integer;
    spawnLocation:      array of Point2D;
    spawnCounter:       Integer;
    pastEnemies:        Integer;
  end;


procedure LoadResources();
function CreateBullets(): Orb;
function CreatePlayer(): Player;
procedure GameCollisions(var currentGame: GameData);
procedure SpawnGenerator(var currentGame: GameData);
procedure SpawnNewEnemies(var currentGame: GameData);
procedure SpawnEnemies(var currentGame: GameData);
procedure StartGame(var currentGame: GameData);
procedure HorizontalPlayerMovement(var currentGame: GameData);
procedure VerticalPlayerMovement(var currentGame: GameData);
procedure DiagonalPlayerMovement(var currentGame: GameData);
procedure PlayerMovement(var currentGame: GameData);
procedure EnemyPlayerCollision(var currentGame: GameData);
procedure EnemyOrbCollision(var currentGame: GameData);
procedure EnemyMovement(var currentGame: GameData);
procedure SpawnDeadEnemies(var currentGame: GameData);
procedure IncreaseEnemyCount(var currentGame: GameData);
procedure OrbPath(var currentGame: GameData);
procedure AngleBetweenPlayerAndMouse(var player: Player);
procedure UpdateOrbs(var currentGame: GameData);
procedure SetFiredOrb(var bullet: Orb; const currentGame: GameData);
procedure FireOrb(var currentGame: GameData);
procedure UpdateGame(var currentGame: GameData);
procedure DrawHealth(const currentGame: GameData);
procedure DrawEnvironment(var currentGame: GameData);
procedure DrawEnemies(var currentGame: GameData);
procedure DrawBullets(var currentGame: GameData);
procedure DrawPlayer(var currentGame: GameData);
procedure DrawGame(var currentGame: GameData);

implementation

// LoadResources:
// Used to load any files that the game will need to function properly
// The materials required in relation to this game are:
// Images for the areana background and the arena boundaries
// Images for the player, bullets and enemies
// Font for drawing the score and time to the screen
procedure LoadResources();
begin
  LoadBitmap('Player', 'Player.png');
  LoadBitmap('Arena', 'Arena.png');
  LoadBitmap('ArenaCoords', 'Arenacoords.png');
  LoadBitmap('Boundaries', 'Boundaries.png');
  LoadBitmap('EnemyBoundaries', 'EnemyBoundaries.png');
  LoadBitmap('Enemy', 'Enemy.png');
  LoadBitmap('Bullet', 'Bullet.png');
  LoadFont('game font', 'arial.ttf');
end;

// CreateBullets:
// This function returns a bullet...
// It creates a bullet sprite using the Bitmap named 'Bullet'
// Sets the bullets location off the screen
// As well as making the visibility False
function CreateBullets(): Orb;
begin
  result.sprite := CreateSprite(BitmapNamed('Bullet'));
  result.xPos := -10;
  result.yPos := -10;
  result.visible := False;
end;

// CreatePlayer:
// This function returns a value of the Player data type
// Sets the player sprite using the Bitmap named 'Player'
// Default staring position is set to the middle of the screen
// Score is set to zero and health to full
// Also calls upon CreateBullets for each element in the bullet array
// This populates all the default bullet data
function CreatePlayer(): Player;
var
  newPlayer: Player;
  i: Integer;
begin
  newPlayer.sprite := CreateSprite(BitmapNamed('Player'));
  newPlayer.xPos := Round(ScreenWidth() / 2 - BitmapWidth(BitmapNamed('Player')) / 2);
  newPlayer.yPos := Round(ScreenHeight() / 2 - BitmapHeight(BitmapNamed('Player')) / 2);
  newPlayer.score := 0;
  newPlayer.health := 3;

  for i := Low(newPlayer.bullet) to High(newPlayer.bullet) do
  begin
    newPlayer.bullet[i] := CreateBullets();
  end;

  result := newPlayer;
end;

// SpawnGenerator:
// Used to set the Point2D data of the seven spawn points in game
// Enemies will randomly spawn from one of the seven points
// In game, you can see these points by pressing the 'D' key
procedure SpawnGenerator(var currentGame: GameData);
begin
  SetLength(currentGame.spawnLocation, 7);

  currentGame.spawnLocation[0].x := 287; currentGame.spawnLocation[0].y := 68;
  currentGame.spawnLocation[1].x := 531; currentGame.spawnLocation[1].y := 55;
  currentGame.spawnLocation[2].x := 682; currentGame.spawnLocation[2].y := 92;
  currentGame.spawnLocation[3].x := 737; currentGame.spawnLocation[3].y := 466;
  currentGame.spawnLocation[4].x := 451; currentGame.spawnLocation[4].y := 501;
  currentGame.spawnLocation[5].x := 128; currentGame.spawnLocation[5].y := 476;
  currentGame.spawnLocation[6].x := 81; currentGame.spawnLocation[6].y := 269;

end;

// SpawnEnemies:
// This procedure is only used once in the game, to spawn the initial group of enemies
// Which appear when the game is launched. For each enemy:
// A sprite is set using the Bitmap name 'Enemy'
// A spawn location is picked at random from one of the seven spawn locations generated by SpawnGenerator
procedure SpawnEnemies(var currentGame: GameData);
var
  i: Integer;
begin
  for i := Low(currentGame.sessionEnemies) to High(currentGame.sessionEnemies) do
  begin
    currentGame.locationKey := Random(7);
    currentGame.sessionEnemies[i].sprite := CreateSprite(BitmapNamed('Enemy'));
    currentGame.sessionEnemies[i].xPos := currentGame.spawnLocation[currentGame.locationKey].x;
    currentGame.sessionEnemies[i].yPos := currentGame.spawnLocation[currentGame.locationKey].y;
    currentGame.sessionEnemies[i].visible := True;
  end;
end;

// SpawnNewEnemies:
// Functionality is identical to SpawnEnemies in the sense that:
// A sprite is set using the Bitmap name 'Enemy' for each enemy
// A spawn location is picked at random from one of the seven spawn locations generated by SpawnGenerator
// However, this procedure is used to spawn more enemies as the game timer ticks away
// A basic difficulty incrementation
// When the array of enemies is changed in size (larger), this procedure sets up each enemy that didn't
// Exist prior to the array length change
procedure SpawnNewEnemies(var currentGame: GameData);
var
  i: Integer;
begin
  for i := High(currentGame.sessionEnemies) downto (currentGame.pastEnemies) do
  begin
    currentGame.locationKey := Random(7);
    currentGame.sessionEnemies[i].sprite := CreateSprite(BitmapNamed('Enemy'));
    currentGame.sessionEnemies[i].xPos := currentGame.spawnLocation[currentGame.locationKey].x;
    currentGame.sessionEnemies[i].yPos := currentGame.spawnLocation[currentGame.locationKey].y;
    currentGame.sessionEnemies[i].visible := True;
  end;
end;

// StartGame:
// Is used once in the execution of this program to set all the initial data in relation to
// The player and enemies
// It calls upon the following procedures which are documented within:
// - SetUpEnemies
// - SpawnGenerator
// - SpawnEnemies
procedure StartGame(var currentGame: GameData);
begin
  currentGame.displayCoordsCount := 1;
  currentGame.spawnCounter := Random(3) + 1;
  currentGame.pastEnemies := currentGame.spawnCounter;
  currentGame.sessionPlayer := CreatePlayer();
  SetLength(currentGame.sessionEnemies, currentGame.spawnCounter);
  SpawnGenerator(currentGame);
  SpawnEnemies(currentGame);
end;

// HorizontalPlayerMovement:
// Used to control the players horizontal movement in game
// The boundaries for the players movement are included
// If the player collides with the arena along a horizontal line, their movement will cease
procedure HorizontalPlayerMovement(var currentGame: GameData);
begin
  if KeyDown(RIGHT_KEY) then
    begin
      currentGame.sessionPlayer.geometry.playerPoint.x += PLAYER_SPEED;
      if BitmapCollision(BitmapNamed('Player'), currentGame.sessionPlayer.geometry.playerPoint, BitmapNamed('Boundaries'), currentGame.sessionPlayer.geometry.boundaryPoint) = False then
        currentGame.sessionPlayer.xPos += PLAYER_SPEED;
    end;
  if KeyDown(LEFT_KEY) then
    begin
      currentGame.sessionPlayer.geometry.playerPoint.x -= PLAYER_SPEED;
      if BitmapCollision(BitmapNamed('Player'), currentGame.sessionPlayer.geometry.playerPoint, BitmapNamed('Boundaries'), currentGame.sessionPlayer.geometry.boundaryPoint) = False then
        currentGame.sessionPlayer.xPos -= PLAYER_SPEED;
    end;
end;

// VerticalPlayerMovement:
// Used to control the players vertical movement in game
// The boundaries for the players movement are included
// If the player collides with the arena along a vertical line, their movement will cease
procedure VerticalPlayerMovement(var currentGame: GameData);
begin
  if KeyDown(UP_KEY) then
    begin
      currentGame.sessionPlayer.geometry.playerPoint.y -= PLAYER_SPEED;
      if BitmapCollision(BitmapNamed('Player'), currentGame.sessionPlayer.geometry.playerPoint, BitmapNamed('Boundaries'), currentGame.sessionPlayer.geometry.boundaryPoint) = False then
        currentGame.sessionPlayer.yPos -= PLAYER_SPEED;
    end;
  if KeyDown(DOWN_KEY) then
    begin
      currentGame.sessionPlayer.geometry.playerPoint.y += PLAYER_SPEED;
      if BitmapCollision(BitmapNamed('Player'), currentGame.sessionPlayer.geometry.playerPoint, BitmapNamed('Boundaries'), currentGame.sessionPlayer.geometry.boundaryPoint) = False then
        currentGame.sessionPlayer.yPos += PLAYER_SPEED;
    end;
end;

// DiagonalPlayerMovement:
// Used to control the players diagonal movements in game movement
// There are no boundaries implemented in diagonal movement
// On account of when the player is moving horizontaly or verticaly and they collide with the arena
// Their movement will cease
procedure DiagonalPlayerMovement(var currentGame: GameData);
begin
  if KeyDown(LEFT_KEY) and KeyDown(DOWN_KEY) then;
  begin
    currentGame.sessionPlayer.xPos -= sqrt(Exp(2 * Ln(PLAYER_SPEED)) / 2);
    currentGame.sessionPlayer.yPos += sqrt(Exp(2 * Ln(PLAYER_SPEED)) / 2);
  end;
  if KeyDown(LEFT_KEY) and KeyDown(UP_KEY) then;
  begin
    currentGame.sessionPlayer.xPos -= sqrt(Exp(2 * Ln(PLAYER_SPEED)) / 2);
    currentGame.sessionPlayer.yPos -= sqrt(Exp(2 * Ln(PLAYER_SPEED)) / 2);
  end;
  if KeyDown(RIGHT_KEY) and KeyDown(DOWN_KEY) then;
  begin
    currentGame.sessionPlayer.xPos += sqrt(PLAYER_SPEED);
    currentGame.sessionPlayer.yPos += sqrt(PLAYER_SPEED);
  end;
  if KeyDown(RIGHT_KEY) and KeyDown(UP_KEY) then;
  begin
    currentGame.sessionPlayer.xPos += sqrt(PLAYER_SPEED);
    currentGame.sessionPlayer.yPos -= sqrt(PLAYER_SPEED);
  end;
end;

// PlayerMovement:
// Calls upon the Horizontal, Veritcal and Diagonal player movement
// Procedures in order to control the players location on screen
// In responce to movement keys being pressed
procedure PlayerMovement(var currentGame: GameData);

begin
  currentGame.sessionPlayer.geometry.playerPoint.x := currentGame.sessionPlayer.xPos;
  currentGame.sessionPlayer.geometry.playerPoint.y := currentGame.sessionPlayer.yPos;
  currentGame.sessionPlayer.geometry.boundaryPoint.x := 0;
  currentGame.sessionPlayer.geometry.boundaryPoint.y := 0;

  HorizontalPlayerMovement(currentGame);
  VerticalPlayerMovement(currentGame);
  DiagonalPlayerMovement(currentGame);
end;

// EnemeyPlayerCollision
// This procedure is used to detect any collisions between the player
// and the enemies in game. If the player collides with an enemy
// They lose life and the enemey is destroyed.
procedure EnemyPlayerCollision(var currentGame: GameData);
var
	i: Integer;
	playerPoint: Point2D;
	enemyPoint: Point2D;

begin
	playerPoint.x := currentGame.sessionPlayer.xPos;
	playerPoint.y := currentGame.sessionPlayer.yPos;

	for i := Low(currentGame.sessionEnemies) to High(currentGame.sessionEnemies) do
	begin
		enemyPoint.x := currentGame.sessionEnemies[i].xPos;
    	enemyPoint.y := currentGame.sessionEnemies[i].yPos;
		if BitmapCollision(BitmapNamed('Player'), playerPoint, BitmapNamed('Enemy'), enemyPoint) then
		begin
		currentGame.sessionEnemies[i].visible := False;
		currentGame.sessionPlayer.health -= 1;
		end;
	end;
end;

// EnemyOrbCollision
// Used to detect whether a bullet has collided with an enemy in game
// When a bullet collides with an enemy
// The player gains a score increase, the enemy dies
// And the bullet in question has is destroyed
procedure EnemyOrbCollision(var currentGame: GameData);
var
  i, j: Integer;
  enemyPoint: Point2D;
  bulletPoint: Point2D;

begin
  for i := Low(currentGame.sessionEnemies) to High(currentGame.sessionEnemies) do
  begin
    enemyPoint.x := currentGame.sessionEnemies[i].xPos;
    enemyPoint.y := currentGame.sessionEnemies[i].yPos;
    for j := Low(currentGame.sessionPlayer.bullet) to High(currentGame.sessionPlayer.bullet) do
    begin
      bulletPoint.x := currentGame.sessionPlayer.bullet[j].xPos;
      bulletPoint.y := currentGame.sessionPlayer.bullet[j].yPos;
      if BitmapCollision(BitmapNamed('Bullet'), bulletPoint, BitmapNamed('Enemy'), enemyPoint) and (currentGame.sessionPlayer.bullet[j].visible) then
      begin
        currentGame.sessionEnemies[i].visible := False;
        currentGame.sessionPlayer.bullet[j].visible := False;
        currentGame.sessionPlayer.score += 10;
      end;
    end;
  end;
end;

// GameCollisions:
// Relies upon EnemyPlayerCollision and EnemyOrbCollision
// To detect whether any in game collisions in regards to the player
// And enemies have occured
procedure GameCollisions(var currentGame: GameData);
begin
	EnemyPlayerCollision(currentGame);
	EnemyOrbCollision(currentGame);
end;

// EnemyMovement
// Used to determine the path of movement for each enemy in game
// The enemies are coded to follow the player, wherever the player is
// This was once of he more difficult procedures to implement
// And required some mathematical research.
// Laws used here include Pythag and basic ratio calculations
procedure EnemyMovement(var currentGame: GameData);
var
  xDistance, yDistance, hypot, ratio : Double;
  i: Integer;
  boundaryPoint: Point2D;
  enemyPoint: Point2D;
begin
  boundaryPoint.x := 0;
  boundaryPoint.y := 0;
  for i := Low(currentGame.sessionEnemies) to High(currentGame.sessionEnemies) do
  begin
    enemyPoint.x := currentGame.sessionEnemies[i].xPos;
    enemyPoint.y := currentGame.sessionEnemies[i].yPos;
    xDistance := currentGame.sessionPlayer.xPos - currentGame.sessionEnemies[i].xPos;
    yDistance := currentGame.sessionPlayer.yPos - currentGame.sessionEnemies[i].yPos;
    hypot := sqrt(xDistance * xDistance + yDistance * yDistance);
    ratio := hypot / ENEMY_SPEED;
    enemyPoint.x += xDistance / ratio;
    enemyPoint.y += yDistance / ratio;
    if BitmapCollision(BitmapNamed('Enemy'), enemyPoint, BitmapNamed('EnemyBoundaries'), boundaryPoint) = False then
    begin
      currentGame.sessionEnemies[i].xPos += xDistance / ratio;
      currentGame.sessionEnemies[i].yPos += yDistance / ratio;
    end;
  end;
end;

// SpawnDeadEnemies
// Used to spawn any enemies which have been destroyed by the player
// Enemies can be killed by contact with bullets or with contact by the player
// This procedure checks to see if an enemies visibility is false, and if it is
// They are respawned
procedure SpawnDeadEnemies(var currentGame: GameData);
var
  i: Integer;
begin
  for i := Low(currentGame.sessionEnemies) to High(currentGame.sessionEnemies) do
  begin
    if currentGame.sessionEnemies[i].visible = False then
    begin
      currentGame.locationKey := Random(7);
      currentGame.sessionEnemies[i].xPos := currentGame.spawnLocation[currentGame.locationKey].x;
      currentGame.sessionEnemies[i].yPos := currentGame.spawnLocation[currentGame.locationKey].y;
      currentGame.sessionEnemies[i].visible := True;
    end;
  end;
end;

// IncreaseEnemyCount:
// This procedure is responcible for incrementing the number of
// enemies as the in game time wears on
// After 5 seconds, there will be five enemies on the arena
// After 20 seconds, there will be fifteen enemies
// After 35 seconds, there will be thirty-five enemies
// After 50 seconds, there will be fifty-five enemies
procedure IncreaseEnemyCount(var currentGame: GameData);
var
  timerTickCount: Integer;
begin
  timerTickCount := TimerTicks(TimerNamed('Game Time'));
  if (timerTickCount > 5000) and (Length(currentGame.sessionEnemies) < 5) then
  begin
    currentGame.spawnCounter := Random(5) + 4;
    SetLength(currentGame.sessionEnemies, currentGame.spawnCounter);
    SpawnNewEnemies(currentGame);
    currentGame.pastEnemies := currentGame.spawnCounter;
  end
  else if (timerTickCount > 20000) and (Length(currentGame.sessionEnemies) < 10) then
  begin
    currentGame.spawnCounter := Random(10) + 9;
    SetLength(currentGame.sessionEnemies, currentGame.spawnCounter);
    SpawnNewEnemies(currentGame);
    currentGame.pastEnemies := currentGame.spawnCounter;
  end
  else if (timerTickCount > 35000) and (Length(currentGame.sessionEnemies) < 20) then
  begin
    currentGame.spawnCounter := Random(20) + 19;
    SetLength(currentGame.sessionEnemies, currentGame.spawnCounter);
    SpawnNewEnemies(currentGame);
    currentGame.pastEnemies := currentGame.spawnCounter;
  end
  else if (timerTickCount > 50000) and (Length(currentGame.sessionEnemies) < 30) then
  begin
    currentGame.spawnCounter := Random(30) + 29;
    SetLength(currentGame.sessionEnemies, currentGame.spawnCounter);
    SpawnNewEnemies(currentGame);
    currentGame.pastEnemies := currentGame.spawnCounter;
  end;
end;

// OrbPath:
// The bullets used in game travel in the direction of where the mouse was clicked to fire
// The purpose of this procedure is to implement that direction
// Basic maths was used to calculate the ratios required for the movement direction
procedure OrbPath(var currentGame: GameData);
var
  i: Integer;
  bulletPoint: Point2D;
  boundaryPoint: Point2D;

begin
  boundaryPoint.x := 0;
  boundaryPoint.y := 0;
  for i := Low(currentGame.sessionPlayer.bullet) to High(currentGame.sessionPlayer.bullet) do
  begin
    bulletPoint.x := currentGame.sessionPlayer.bullet[i].xPos;
    bulletPoint.y := currentGame.sessionPlayer.bullet[i].yPos;
    currentGame.sessionPlayer.bullet[i].xPos += currentGame.sessionPlayer.bullet[i].data.xDistance / currentGame.sessionPlayer.bullet[i].data.moveRatio;
    currentGame.sessionPlayer.bullet[i].yPos += currentGame.sessionPlayer.bullet[i].data.yDistance / currentGame.sessionPlayer.bullet[i].data.moveRatio;
    if BitmapCollision(BitmapNamed('Bullet'), bulletPoint, BitmapNamed('Boundaries'), boundaryPoint) then
    begin
      currentGame.sessionPlayer.bullet[i].visible := False;
    end;
  end;
end;

procedure AngleBetweenPlayerAndMouse(var player: Player);

var
  diffPointX, diffPointY: Double;
begin
  diffPointX := (MouseX() - (player.xPos + (SpriteWidth(player.sprite) / 2)));
  diffPointY := ((player.yPos + (SpriteHeight(player.sprite) / 2)) - MouseY());

  if (diffPointX = 0) and (diffPointY >= 0) then
    player.angle := 90
  else if (diffPointX = 0) and (diffPointY <= 0) then
    player.angle := 270
  else if (diffPointX < 0) then
    player.angle := Round(ArcTan(diffPointY / diffPointX) / Pi * 180 + 180)
  else
  begin
    player.angle := Round(ArcTan(diffPointY / diffPointX) / Pi * 180);
    if (player.angle < 0) then
      player.angle := 360 - (player.angle * -1);
  end;
end;

// UpdateOrbs:
// All bullets fire in game must move in the direction in which the mouse was clicked
// So for each bullet that is fired, the OrbPath procedure is called to set their paths
procedure UpdateOrbs(var currentGame: GameData);
var
  i: Integer;

begin
  for i := Low(currentGame.sessionPlayer.bullet) to High(currentGame.sessionPlayer.bullet) do
  begin
    if currentGame.sessionPlayer.bullet[i].visible then
    begin
      OrbPath(currentGame);
      break;
    end;
  end;
end;

// SetFireOrb:
// When a bullet is fired in game, this procedure is used to set their location
// Based on where the user clicked the mouse
// It calculates all the maths required by OrbPath in order to move
// The bullet in the direction of the mouse until it moves off screen
procedure SetFiredOrb(var bullet: Orb; const currentGame: GameData);
begin
  bullet.visible := True;
  bullet.xPos := Round(currentGame.sessionPlayer.xPos + BitmapWidth(BitmapNamed('Player')) / 2 - BitmapWidth(BitmapNamed('Bullet')) / 2);
  bullet.yPos := currentGame.sessionPlayer.yPos + BitmapWidth(BitmapNamed('Player')) / 2 - BitmapHeight(BitmapNamed('Bullet')) / 2;
  bullet.data.xDistance := MouseX() - bullet.xPos;
  bullet.data.yDistance := MouseY() - bullet.yPos;
  bullet.data.hypot := sqrt(bullet.data.xDistance * bullet.data.xDistance + bullet.data.yDistance * bullet.data.yDistance);
  bullet.data.moveRatio := bullet.data.hypot / ORB_SPEED;
end;

// FireOrb
// Calls upon SetFiredOrb each time the left mouse button is clicked
// SetFiredOrb calculates all the movement data required for each bullet
procedure FireOrb(var currentGame: GameData);
var
  i: Integer;

begin
  if MouseClicked(LEFT_BUTTON) then
  begin
    for i := Low(currentGame.sessionPlayer.bullet) to High(currentGame.sessionPlayer.bullet) do
    begin
      if not currentGame.sessionPlayer.bullet[i].visible then
      begin
        SetFiredOrb(currentGame.sessionPlayer.bullet[i], currentGame);
        break;
      end;
    end;
  end;
end;

// UpdateGame:
// Calls upon the procedures which are required to update any in game elements
// These procedures include:
// PlayerMovement
// FireOrb
// UpdateOrbs
// EnemyMovement
// GameCollisions
// SpawnDeadEnemies
// IncreaseEnemyCount
procedure UpdateGame(var currentGame: GameData);
begin
  if KeyTyped(D_KEY) then
    begin
      currentGame.displayCoordsCount += 1;
    end;
  PlayerMovement(currentGame);
  FireOrb(currentGame);
  UpdateOrbs(currentGame);
  EnemyMovement(currentGame);
  GameCollisions(currentGame);
  IncreaseEnemyCount(currentGame);
  SpawnDeadEnemies(currentGame);
end;

// DrawEnvironment:
// Used to draw the elements which consist of the game environment
// Including the arena background, the debug screen
// Score, time and the enemy boundaries
procedure DrawEnvironment(var currentGame: GameData);
var
  timerCount: String;
  score: String;

begin
  timerCount := IntToStr(TimerTicks(TimerNamed('Game Time')));
  score := IntToStr(currentGame.sessionPlayer.score);
  DrawBitmap('EnemyBoundaries', 0, 0);
  DrawBitmap('Boundaries', 0, 0);
  DrawBitmap('Arena', 0, 0);

  if (currentGame.displayCoordsCount mod 2 = 0) then
    DrawBitmap('ArenaCoords', 0, 0);

  DrawText(timerCount, ColorWhite, 'game font', 10, 3, 3);
  DrawText(score, ColorWhite, 'game font', 10, 10, 3);
end;

// DrawHealth:
// Used to display the current health the player has to the screen
procedure DrawHealth(const currentGame: GameData);
var
	i: Integer;
begin
	for i := 1 to MAX_LIVES do
	begin
		DrawCircle(ColorWhite, BitmapWidth(BitmapNamed('Player')) + (i * BitmapWidth(BitmapNamed('Player')) + 10 * i), ScreenHeight() - BitmapHeight(BitmapNamed('Player')) - 10, Round(BitmapHeight(BitmapNamed('Player')) / 2));

		if (i <= currentGame.sessionPlayer.health) then
			DrawBitmap(BitmapNamed('Player'), BitmapWidth(BitmapNamed('Player')) - BitmapWidth(BitmapNamed('Player')) / 2 + (i * BitmapWidth(BitmapNamed('Player')) + 10 * i), ScreenHeight() - BitmapHeight(BitmapNamed('Player')) - BitmapHeight(BitmapNamed('Player')) / 2 - 10);
	end;
end;

// DrawEnemies:
// Draws each of the active in game enemies to the screen
procedure DrawEnemies(var currentGame: GameData);
var
  i: Integer;
begin
  for i := Low(currentGame.sessionEnemies) to High(currentGame.sessionEnemies) do
  begin
    if (currentGame.sessionEnemies[i].visible = True) then
    begin
      SpriteSetX(currentGame.sessionEnemies[i].sprite, currentGame.sessionEnemies[i].xPos);
      SpriteSetY(currentGame.sessionEnemies[i].sprite, currentGame.sessionEnemies[i].yPos);
      DrawSprite(currentGame.sessionEnemies[i].sprite);
    end;
  end;
end;

// DrawBullets:
// Draws each of the activ in game bullets to the screen
procedure DrawBullets(var currentGame: GameData);
var
  i: Integer;
begin
  for i := Low(currentGame.sessionPlayer.bullet) to High(currentGame.sessionPlayer.bullet) do
  begin
    if (currentGame.sessionPlayer.bullet[i].visible = True) then
    begin
      SpriteSetX(currentGame.sessionPlayer.bullet[i].sprite, currentGame.sessionPlayer.bullet[i].xPos);
      SpriteSetY(currentGame.sessionPlayer.bullet[i].sprite, currentGame.sessionPlayer.bullet[i].yPos);
      DrawSprite(currentGame.sessionPlayer.bullet[i].sprite);
    end;
  end;
end;

// DrawPlayer:
// Draws the player to the screen
procedure DrawPlayer(var currentGame: GameData);
begin
  SpriteSetX(currentGame.sessionPlayer.sprite, currentGame.sessionPlayer.xPos);
  SpriteSetY(currentGame.sessionPlayer.sprite, currentGame.sessionPlayer.yPos);
  DrawSprite(currentGame.sessionPlayer.sprite);
end;

// DrawGame:
// Calls upon each of the procedures which are used to draw
// All the in game elements to the screen. They are:
// DrawEnvironment
// DrawEnemies
// DrawBullets
// DrawPlayer
// DrawHealth
procedure DrawGame(var currentGame: GameData);
begin
  DrawEnvironment(currentGame);
  DrawEnemies(currentGame);
  DrawBullets(currentGame);
  DrawPlayer(currentGame);
  DrawHealth(currentGame);
end;

end.
