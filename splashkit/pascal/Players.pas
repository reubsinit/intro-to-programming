unit Players;SplashKit

interface
	uses SwinGame, sgTypes, math, Globals;

	function CreatePlayer(x, y, speed: Double; playerBitmap: Bitmap): Player;
	procedure SetControls(var player: Player; newLeftKey, newRightKey, newUpKey, newDownKey: KeyCode);
	procedure PointPlayerAt(var player: Player; x, y: Double);
	procedure DrawPlayer(player: Player);
	procedure ProcessPlayerEvents(var gData: GameData);

implementation
	function CreatePlayer(x, y, speed: Double; playerBitmap: Bitmap): Player;
	begin
		result.x := x;
		result.y := y;
		result.speed := speed;
		result.sprite := CreateSprite(playerBitmap);
	end;

	procedure SetControls(var player: Player; newLeftKey, newRightKey, newUpKey, newDownKey: KeyCode);
	begin
		player.leftKey := newLeftKey;
		player.rightKey := newRightKey;
		player.upKey := newUpKey;
		player.downKey := newDownKey;
	end;

	procedure PointPlayerAt(var player: Player; x, y: Double);
	var
		dX, dY: Double;
	begin
		dX := Round(x - (SpriteX(player.sprite) + (SpriteWidth(player.sprite) / 2))); // Adding half the sprite width centers our orientation.
		dY := Round((SpriteY(player.sprite) + (SpriteHeight(player.sprite) / 2)) - y); // Adding half the sprite height centers our orientation.

		if (dX = 0) and (dY >= 0) then
			player.angle := 90
		else if (dX = 0) and (dY <= 0) then
			player.angle := 270
		else if (dX < 0) then
			player.angle := Round(ArcTan(dY / dX) / Pi * 180 + 180)
		else
			player.angle := Round(ArcTan(dY / dX) / Pi * 180);
	end;

	procedure DrawPlayer(player: Player);
	begin
		SpriteSetRotation(player.sprite, player.angle);
		SpriteSetX(player.sprite, player.x);
		SpriteSetY(player.sprite, player.y);
		DrawSprite(player.sprite);
	end;

	procedure DrawShots(var gData: GameData; shots, spread, distance: Integer);
	var
		angle, randomNo, startX, startY, endX, endY: Double;
		i, ii: Integer;
		shot: LineSegment;
	begin
		for i := 0 to (shots - 1) do
		begin
			// Generate a negative or positive number?
			randomNo := Round(Random());

			if randomNo = 0 then
				angle := gData.activePlayer.angle - Random() * Spread
			else
				angle := gData.activePlayer.angle + Random() * Spread;

			startX := gData.activePlayer.x + SpriteWidth(gData.activePlayer.sprite) / 2;
			startY := gData.activePlayer.y + SpriteHeight(gData.activePlayer.sprite) / 2;
			endX := gData.activePlayer.x + SpriteWidth(gData.activePlayer.sprite) / 2 + distance * Cos(angle * Pi / 180);
			endY := gData.activePlayer.y + SpriteHeight(gData.activePlayer.sprite) / 2 + distance * Sin(angle * Pi / 180) * -1;

			//Writeln('A: ', Round(100 * Cos(gData.activePlayer.angle * Pi / 180)),
			//' B: ', Round(100 * Sin(gData.activePlayer.angle * Pi / 180)),
			//' Player Angle: ', Round(gData.activePlayer.angle));

			DrawLine(ColorWhite, startX, startY, endX, endY);

			// Collision checking.
			for ii := Low(gData.activeEnemies) to High(gData.activeEnemies) do
			begin

				shot := LineFrom(startX, startY, endX, endY);

				if CircleLineCollision(gData.activeEnemies[ii].sprite, shot) and gData.activeEnemies[ii].alive then
				begin
					gData.score += 1;

					//Writeln(High(gData.blood));

					gData.activeEnemies[ii].alive := false;
				end;
			end;
		end;
	end;

	procedure ProcessPlayerEvents(var gData: GameData);
	var
		left, right, up, down: Boolean;
	begin
		left := false;
		right := false;
		up := false;
		down := false;

		// Are we shooting?
		if MouseClicked(LEFT_BUTTON) then
		begin
			DrawShots(gData, 6, 15, 200);
		end;

		// Point at mouse.
		PointPlayerAt(gData.activePlayer, Round(MouseX) + CameraX(), Round(MouseY) + CameraY());

		// Check for keyboard input and move the circle.
		if KeyDown(gData.activePlayer.leftKey) then
			left := true;
		if KeyDown(gData.activePlayer.rightKey) then
			right := true;
		if KeyDown(gData.activePlayer.upKey) then
			up := true;
		if KeyDown(gData.activePlayer.downKey) then
			down := true;

		if left and (up or down) then
		begin
			gData.activePlayer.x -= Sqrt(Exp(2 * Ln(gData.activePlayer.speed)) / 2);
		end
		else if left then
		begin
			gData.activePlayer.x -= gData.activePlayer.speed;
		end;

		if right and (up or down) then
		begin
			gData.activePlayer.x += Sqrt(Exp(2 * Ln(gData.activePlayer.speed)) / 2);
		end
		else if right then
		begin
			gData.activePlayer.x += gData.activePlayer.speed;
		end;

		if up and (left or right) then
		begin
			gData.activePlayer.y -= Sqrt(Exp(2 * Ln(gData.activePlayer.speed)) / 2);
		end
		else if up then
		begin
			gData.activePlayer.y -= gData.activePlayer.speed
		end;

		if down and (left or right) then
		begin
			gData.activePlayer.y += Sqrt(Exp(2 * Ln(gData.activePlayer.speed)) / 2);
		end
		else if down then
		begin
			gData.activePlayer.y += gData.activePlayer.speed;
		end;
	end;
end.
