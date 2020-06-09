program AlienInvaders;
uses SplashKit, SysUtils;

const
	PLAYER_SPEED = 5;
	BULLET_SPEED = 10;
	ALIEN_SPEED = 3;
	MAX_ALIENS = 10;
	MAX_BULLETS = 5;
	MAX_EXPLOSIONS = MAX_BULLETS;
	BOSS_HEALTH = 4;

type
	BulletData = record
		x, y: Integer;
		isActive: Boolean;
	end;

	AlienKind = ( Invader, Boss );

	AlienData = record
		x, y: Integer;
		kind: AlienKind;
		life: Integer;
	end;

	PlayerData = record
		x, y: Integer;
		score: Integer;
	end;

	ExplosionData = record
		sprt: Sprite;
		active: Boolean;
	end;

	AlienInvadersData = record
		player: PlayerData;
		aliens: array [0..MAX_ALIENS-1] of AlienData;
		bullets: array [0..MAX_BULLETS-1] of BulletData;

		explosions: array [0..MAX_EXPLOSIONS-1] of ExplosionData;
	end;

//
// Load the resources (images and sounds)
//
procedure LoadResources();
begin
	LoadBitmap('background', 'background.jpg');
	LoadBitmap('ship', 'spaceship-md.png');
	LoadBitmap('bullet', 'missile-sprite.png');
	LoadBitmap('boss', 'Alien.png');
	LoadBitmap('alien', 'Alien1.png');
	LoadBitmap('explode', 'red_explosion.png');

	// Set the size of the cells in the bitmap
	// - 38 wide, 38 high, 8 columns, 2 rows, 16 images
	BitmapSetCellDetails(BitmapNamed('explode'), 38, 38, 8, 2, 16);

	LoadSoundEffect('fire', 'Missile.ogg');
	LoadSoundEffect('hit', 'Destroy.wav');
	LoadSoundEffect('laugh', 'Laugh.ogg');

	LoadAnimationScript('RedExplosion', 'RedExplosion.txt');
end;

//
// Returns the bitmap for the indicated Alien based on its kind
//
function AlienBitmap(const alien: AlienData): Bitmap;
begin
	case alien.kind of
		Boss: result := BitmapNamed('boss');
		else result := BitmapNamed('alien');
	end;
end;

//
// Spawn the Alien off the top of the screen.
//
procedure SpawnAlien(var alien: AlienData);
begin
	alien.life := 1;

	// assign the kind
	case Rnd(10) of
		0..1:
		begin
		 	alien.kind := Boss;
		 	alien.life := BOSS_HEALTH;
		end;
		else alien.kind := Invader;
	end;
	// position the Alien
	alien.x := Rnd(ScreenWidth() - BitmapWidth(AlienBitmap(alien)));
	alien.y := -(20 + Rnd(800));
end;

//
// Load Sprites for each explosion.
// - Sprites keep track of the image location,
//   plus animation details etc.
procedure SetupExplosions(var explosions: array of ExplosionData);
var
	i: Integer;
begin
	for i := Low(explosions) to High(explosions) do
	begin
		explosions[i].sprt := CreateSprite(BitmapNamed('explode'), AnimationScriptNamed('RedExplosion'));
		explosions[i].active := false;
	end;
end;

//
// Setup all of the initial values for running the game.
//
procedure SetupGame(var gameData: AlienInvadersData);
var
	i: Integer;
begin
	// Position the player
	gameData.player.x := Round(ScreenWidth() / 2 - BitmapWidth(BitmapNamed('ship')) / 2);
	gameData.player.y := 500;

	// Set score
	gameData.player.score := 0;

	// Setup all of the bullets
	for i := Low(gameData.bullets) to High(gameData.bullets) do
	begin
		gameData.bullets[i].x := 50;
		gameData.bullets[i].y := 50;
		gameData.bullets[i].isActive := false;
	end;

	// Spawn all of the Aliens
	for i := Low(gameData.aliens) to High(gameData.aliens) do
	begin
		SpawnAlien(gameData.aliens[i]);
	end;

	// Setup the explosions
	SetupExplosions(gameData.explosions);
end;

//
// Fire a bullet from the player's location.
//
procedure FireBullet(const player: PlayerData; var toFire: BulletData);
begin
	toFire.isActive := true;
	toFire.x := Round(player.x + BitmapWidth(BitmapNamed('ship')) / 2 - BitmapWidth(BitmapNamed('bullet')) / 2);
	toFire.y := player.y - BitmapHeight(BitmapNamed('bullet'));
	PlaySoundEffect('fire');
end;

//
// Handle the user input that moves the player
//
procedure HandleMovePlayer(var player: PlayerData);
begin
	if KeyDown(LEFT_KEY) then
	begin
		player.x -= PLAYER_SPEED;
	end;

	if KeyDown(RIGHT_KEY) then
	begin
		player.x += PLAYER_SPEED;
	end;

	if KeyDown(UP_KEY) then
	begin
		player.y -= PLAYER_SPEED;
	end;

	if KeyDown(DOWN_KEY) then
	begin
		player.y += PLAYER_SPEED;
	end;
end;

//
// Handle the input the fires bullets
//
procedure HandleFireBullet(const player: PlayerData; var bullets: array of BulletData);
var
	i: Integer;
begin
	if KeyTyped(SPACE_KEY) then
	begin
		for i := Low(bullets) to High(bullets) do
		begin
			if not bullets[i].isActive then
			begin
				FireBullet(player, bullets[i]);
				break;
			end;
		end;
	end;
end;

//
// Listen for input and update the player, and the bullets
//
procedure HandleUserInput(var gameData: AlienInvadersData);
begin
	HandleMovePlayer(gameData.player);
	HandleFireBullet(gameData.player, gameData.bullets);
end;

//
// Test if a bullet has hit an alien
//
function BulletHitAlien(const bullet: BulletData; const alien: AlienData): boolean;
begin
	result := BitmapCollision(
					BitmapNamed('bullet'), bullet.x, bullet.y,
					AlienBitmap(alien), alien.x, alien.y);
end;

//
// Start an explosion at the indicated point
//
procedure StartExplosion(var explosions: array of ExplosionData; const bullet: BulletData);
var
	cx, cy: Integer;
	i: Integer;
begin
	for i := Low(explosions) to High(explosions) do
	begin
		if not explosions[i].active then
		begin
			// Position the sprite on top/middle of bullet (center at cx, cy)
			cx := bullet.x + BitmapWidth(BitmapNamed('bullet')) div 2 - BitmapCellWidth(BitmapNamed('explode')) div 2;
			cy := bullet.y - BitmapCellHeight(BitmapNamed('explode')) div 2;
			// Actually move the sprite here...
			MoveSpriteTo(explosions[i].sprt, cx, cy);

			// Play the animation from the AnimationScript
			SpriteStartAnimation(explosions[i].sprt, 'FireExplosion');

			// Make active
			explosions[i].active := true;

			// Dont keep looking for more places
			break;
		end;
	end;
end;

//
// Test if a bullet has hit an alien
//
procedure CheckBulletCollisions(var gameData: AlienInvadersData);
var
	i, j: Integer;
begin
	// For each bullet ... Use j as bullet index!
	for j := Low(gameData.bullets) to High(gameData.bullets) do
	begin
		// Skip to next bullet if this one is inactive...
		if not gameData.bullets[j].isActive then
			continue; // skip this bullet, continue with the next one

		// Check if this bullet hit an alien... use i as alien index!
		for i := Low(gameData.aliens) to High(gameData.aliens) do
		begin
			if BulletHitAlien(gameData.bullets[j], gameData.aliens[i]) then
			begin
				gameData.bullets[j].isActive := false;
				PlaySoundEffect('hit');
				StartExplosion(gameData.explosions, gameData.bullets[j]);

				gameData.aliens[i].life -= 1;
				if gameData.aliens[i].life <= 0 then
				begin
					SpawnAlien(gameData.aliens[i]);

					gameData.player.score += 5;
				end;

				break;
			end;
		end;
	end;
end;

//
// Move a bullet...
//
procedure UpdateBullet(var bullet: BulletData; var aliens: array of AlienData);
begin
	bullet.y -= BULLET_SPEED;

	if bullet.y < -BitmapHeight(BitmapNamed('bullet')) then
	begin
		// its off the screen... so make it inactive
		bullet.isActive := false;
	end;
end;

//
// Update the active bullets, and check
// if they have hit any of the Aliens
//
procedure UpdateAllBullets(var bullets: array of BulletData; var aliens: array of AlienData);
var
	i: Integer;
begin
	for i := Low(bullets) to High(bullets) do
	begin
		if bullets[i].isActive then
		begin
			UpdateBullet(bullets[i], aliens);
		end;
	end;
end;

//
// Move all of the aliens down
//
procedure UpdateAllAliens(var aliens: array of AlienData; var player: PlayerData);
var
	i: Integer;
begin
	for i := Low(aliens) to High(aliens) do
	begin
		aliens[i].y += ALIEN_SPEED;

		if aliens[i].y > ScreenHeight() then
		begin
			if aliens[i].kind = Boss then
			begin
				player.score -= 100;
				PlaySoundEffect('laugh');
			end;

			SpawnAlien(aliens[i]);
		end;
	end;
end;

//
// Update active explosions, deactivate when animation ends.
//
procedure UpdateExplosions(var explosions: array of ExplosionData);
var
	i: Integer;
begin
	for i := Low(explosions) to High(explosions) do
	begin
		if explosions[i].active then
		begin
			UpdateSprite(explosions[i].sprt);
			if SpriteAnimationHasEnded(explosions[i].sprt) then
			begin
				explosions[i].active := false;
			end;
		end;
	end;
end;

//
// Update the game.
//
procedure UpdateGame(var gameData: AlienInvadersData);
begin
	UpdateAllBullets(gameData.bullets, gameData.aliens);
	UpdateAllAliens(gameData.aliens, gameData.player);

	CheckBulletCollisions(gameData);
	UpdateExplosions(gameData.explosions);
end;

//
// Draw a bullet
//
procedure DrawBullet(const bullet: BulletData);
begin
	if bullet.isActive then
	begin
		DrawBitmap('bullet', bullet.x, bullet.y);
	end;
end;

//
// Draw all of the explosions
//
procedure DrawExplosions(const explosions: array of ExplosionData);
var
	i: Integer;
begin
	for i := Low(explosions) to High(explosions) do
	begin
		if explosions[i].active then
		begin
			DrawSprite(explosions[i].sprt);
		end;
	end;
end;

//
// Draw an Alien
//
procedure DrawAlien(const alien: AlienData);
begin
	DrawBitmap(AlienBitmap(alien), alien.x, alien.y);
end;

//
// Draw the game - player, bullets, aliens
//
procedure DrawGame(const gameData: AlienInvadersData);
var
	i: Integer;
begin
	//ClearScreen(ColorWhite);
	DrawBitmap('background', 0, 0);

	//Draw the player
	DrawBitmap('ship', gameData.player.x, gameData.player.y);

	//For each bullet
	for i := Low(gameData.bullets) to High(gameData.bullets) do
	begin
		DrawBullet(gameData.bullets[i]);
	end;

	//For each alien
	for i := Low(gameData.aliens) to High(gameData.aliens) do
	begin
		DrawAlien(gameData.aliens[i]);
	end;

	DrawExplosions(gameData.explosions);

	DrawText('Score: ' + IntToStr(gameData.player.score), ColorWhite, 0, 0);

	// Limit refresh rate to 60
	RefreshScreen(60);
end;

//
// Runs the game.
//
procedure Main();
var
	gameData: AlienInvadersData;
begin
  	OpenAudio();

  	OpenWindow('Game of Thrones - the SPACE adventure game!', 600, 600);


  	LoadResources();

	SetupGame(gameData);

	//Game loop
	repeat
		ProcessEvents();

		HandleUserInput(gameData);
		UpdateGame(gameData);
		DrawGame(gameData);
	until WindowCloseRequested('Game of Thrones - the SPACE adventure game!');
end;


begin
	Main();
end.
