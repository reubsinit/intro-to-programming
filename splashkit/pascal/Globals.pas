unit Globals;

interface
	uses SplashKit;

	type
		Enemy = record
			x, y, range: Double;
			angle, speed: Integer;
			sprite: Sprite;
			alive: Boolean;
			blood: Sprite;
	end;

	type
		Player = record
			x, y, angle, speed: Double;
			sprite: Sprite;
			leftKey, rightKey, upKey, downKey: KeyCode;
	end;

	type
		GameData = record
			activePlayer: Player;
			activeEnemies: array of Enemy;
			score: Integer;
			bloodBitmaps: array of Bitmap;
	end;

implementation

end.
