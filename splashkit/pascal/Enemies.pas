unit Enemies;

interface
	uses SplashKit, Globals;

	function CreateEnemy(x, y, range: Double; speed: Integer; bitmap: Bitmap): Enemy;
	procedure PointEnemyAt(var enemy: Enemy; x, y: Double);
	procedure ProcessEnemyEvents(var gData: GameData);
	procedure DrawEnemies(const enemies: array of Enemy);

	implementation

	function CreateEnemy(x, y, range: Double; speed: Integer; bitmap: Bitmap): Enemy;
	var
		newEnemy: Enemy;
	begin
		newEnemy.x := x;
		newEnemy.y := y;
		newEnemy.range := range;
		newEnemy.speed := speed;
		newEnemy.alive := true;
		newEnemy.sprite := CreateSprite(bitmap);
		result := newEnemy;
	end;

	procedure PointEnemyAt(var enemy: Enemy; x, y: Double);
	var
		dX, dY: Double;
	begin
		dX := x - (SpriteX(enemy.sprite) + (SpriteWidth(enemy.sprite) / 2)); // Adding half the sprite width centers our orientation.
		dY := (SpriteY(enemy.sprite) + (SpriteHeight(enemy.sprite) / 2)) - y; // Adding half the sprite height centers our orientation.

		if (dX = 0) and (dY >= 0) then
			enemy.angle := 90
		else if (dX = 0) and (dY <= 0) then
			enemy.angle := 270
		else if (dX < 0) then
			enemy.angle := Round(ArcTan(dY / dX) / Pi * 180 + 180)
		else
			enemy.angle := Round(ArcTan(dY / dX) / Pi * 180);
	end;

	procedure ProcessEnemyEvents(var gData: GameData);
	var
		i: Integer;
		a, b, c, modifier: Double;
	begin
		for i := Low(gData.activeEnemies) to High(gData.activeEnemies) do
			begin
				if gData.activeEnemies[i].alive then
				begin
					a := gData.activePlayer.x - gData.activeEnemies[i].x;
					b := gData.activePlayer.y - gData.activeEnemies[i].y;
					c := Sqrt(a * a + b * b);

					//Writeln('offsetX: ', Round(offsetX), ' player width: ', SpriteWidth(player.sprite), ' player X: ', Round(player.x), ' enemy X: ', Round(enemies[i].x));

					// Check if enemy is close enough to the player to begin chasing.
					if (gData.activeEnemies[i].range >= c) then
					begin
						modifier := c / gData.activeEnemies[i].speed;
						gData.activeEnemies[i].x += a / modifier;
						gData.activeEnemies[i].y += b / modifier;
						PointEnemyAt(gData.activeEnemies[i], gData.activePlayer.x, gData.activePlayer.y);
					end;
				end;
		end;
	end;

	procedure DrawEnemies(const enemies: array of Enemy);
	var
		i: Integer;
	begin
		for i := Low(enemies) to High(enemies) do
		begin
			if (enemies[i].x + SpriteWidth(enemies[i].sprite) > CameraX())
				and (enemies[i].x < CameraX() + ScreenWidth())
				and (enemies[i].y + SpriteHeight(enemies[i].sprite) > CameraY())
				and (enemies[i].y < CameraY() + ScreenHeight())
				and enemies[i].alive then
			begin
				SpriteSetRotation(enemies[i].sprite, enemies[i].angle);
				SpriteSetX(enemies[i].sprite, Round(enemies[i].x));
				SpriteSetY(enemies[i].sprite, Round(enemies[i].y));
				DrawSprite(enemies[i].sprite);
			end
			else if (enemies[i].x + SpriteWidth(enemies[i].sprite) > CameraX())
				and (enemies[i].x < CameraX() + ScreenWidth())
				and (enemies[i].y + SpriteHeight(enemies[i].sprite) > CameraY())
				and (enemies[i].y < CameraY() + ScreenHeight())
				and enemies[i].alive <> true then
			begin
				SpriteSetX(enemies[i].blood, Round(enemies[i].x));
				SpriteSetY(enemies[i].blood, Round(enemies[i].y));
				DrawSprite(enemies[i].blood);
			end;
		end;
	end;
end.
