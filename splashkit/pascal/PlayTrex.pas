program Trex;
uses SplashKit, sysutils, TrexLib;

// Main:
// This is where trex all comes together !
procedure Main();
var
  sessionGame: GameData;
begin
  OpenAudio();

  OpenWindow('Trex', 884, 536);

  LoadResources();

  StartGame(sessionGame);

  CreateTimer('Game Time');
  StartTimer(TimerNamed('Game Time'));

//Game loop
  repeat
    ProcessEvents();
    UpdateGame(sessionGame);
    DrawGame(sessionGame);
    RefreshScreen(60);
    ClearScreen(ColorWhite);
  until WindowCloseRequested('Trex');
end;

begin
  Main();
end.
