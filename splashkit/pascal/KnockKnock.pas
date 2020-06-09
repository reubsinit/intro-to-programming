program KnockKnock;
uses SplashKit;

// Create a procedure that can load the resources needed by the program.
// This is called `LoadResources'
procedure LoadResources();
begin
  // Loads a door image (bitmap)
  //  'door' is our name for this image
  //  'KnockKnock.jpg' is the file to load from Resources/images
  LoadBitmap('door', 'KnockKnock.jpg');

  // Loads my custom splash image (bitmap)
  //  'aapsplash' is our name for this image
  //  'AnguishSplash.jpg' is the file to load from Resources/images
  LoadBitmap('aapsplash', 'AnguishSplash.jpg');

  // Loads a sound which is used in conjunction with my splash screen
  //  'aapsplashsound' is our name for this sound effect
  //  'splashsound.wav' is the file to load from Resources/sounds
  LoadSoundEffect('aapsplashsound', 'splashsound.wav');

  // Loads a knocking sound (sound effect)
  //  'knock' is our name for this sound effect
  //  'door-knock-3.wav' is the file to load from Resources/sounds
  LoadSoundEffect('knock', 'door-knock-3.wav');

  // Loads a laughing sound (sound effect)
  //  'laugh' is our name for this sound effect
  //  'slowlaugh.wav' is the file to load from Resources/sounds
  LoadSoundEffect('laugh', 'slowlaugh.wav');

  // Loads a font for the joke text
  //  'joke font' is our name for this font
  //  'Action Man.ttf' is the file to load from Resources/fonts
  LoadFont('joke font', 'Action Man.ttf');
end;

// Create a procedure that draws the my splash screen image and plays the intro-music, and waits so the user sees it.
// This is called `DrawMySplashScreen'.
procedure DrawMySplashScreen();
begin
  // Call PlaySoundEffect ->
  // Call DrawBitmap ->
  // Play the intro-music for my custom splash 'aapsplashsound'.
  //    Draw the 'aapsplash' image on the screen
  //    First 0 is the x value (left)
  //    Next 0 is the y value (top)
  PlaySoundEffect('aapsplashsound');
  DrawBitmap('aapsplash', 0, 0);

  // Call RefreshScreen ->
  //    Show the 'current screen' to the user
  RefreshScreen(60);

  // Call Delay ->
  //    Wait for 11 seconds (11000 ms)
  Delay(11000);
end;

// Create a procedure that draws the door, and waits so the user sees it.
// This is called `DrawDoor'.
procedure DrawDoor();
begin
  // Call DrawBitmap ->
  //    Draw the 'door' image on the screen
  //    First 0 is the x value (left)
  //    Next 0 is the y value (top)
  DrawBitmap('door', 0, 0);

  // Call RefreshScreen ->
  //    Show the 'current screen' to the user
  RefreshScreen(60);

  // Call Delay ->
  //    Wait for 2 seconds (2000 ms)
  Delay(2000);
end;

// Create a procedure that will clear an area for the knock knock jokes text
// This is called 'ClearAreaForText'.
procedure ClearAreaForText();
begin
  // Draw a rectangle on the screen.
  // The rectangle is black.
  // The 0 represents the position of the rectangle horizontally on the screen.
  // The 450 represents the position of the rectangle vertically on the screen
  // 800 specifies how wide the rectangle is.
  // 150 specifies how high the rectangle is.
  FillRectangle(ColorBlack, 0, 450, 800, 150);

  // RefreshScreen is called to show the current screen i.e. the screen which has the black rectangle on it.
  RefreshScreen(60);

  // Delay is called with a value of 500ms, or half of a second.
  Delay(500);
end;

// Create a procedure that will print the joke to the screen.
// This is called 'LinePrint'.
// This procedure accepts parameters in the form of a string.
procedure LinePrint(text: String);
begin
  // Draws the joke text as White, using the 'joke font' at the position on the screen noted by 200, 500.
  // Show the current screen.
  // Give the user 2000ms to read the current line of the joke
  // Then call the procedure ClearAreaForText which as we know prints a black rectangle over the last piece of text.
  DrawText(text, ColorWhite, 'joke font', 48, 200, 500);
  RefreshScreen(60);
  Delay(2000);
  ClearAreaForText();
end;

// Create a procedure that will print the punchline to the screen.
// This is called 'PunchLine'.
// This procedure accepts parameters in the form of a string.
procedure PunchLine(text: String);
begin
  // Draws the joke text as White, using the 'joke font' at the position on the screen noted by 200, 500.
  // Show the current screen.
  // Play the punch line laugh.
  // Give the user 2000ms, or 2 seconds to thouroughly enjoy my fantastic humor
  // Then call the procedure ClearAreaForText which as we know prints a black rectangle over the last piece of text.
  DrawText(text, ColorWhite, 'joke font', 48, 200, 500);
  RefreshScreen(60);
  PlaySoundEffect('laugh');
  Delay(2000);
  ClearAreaForText();
end;

// Create a procedure that will present the 'Knock Knock' joke to the user.
// This is called 'ShowKnockKnock'.
procedure ShowKnockKnock();
begin
  // Procedure call. In this instance, the procedure 'ClearAreaForText' is called.
  ClearAreaForText();
  // Procedure call. In this instance, the procedure is being called from the SplashKit library.
  // This procedure plays a sound.
  // In this case, the sound is called 'knock', which has been asssociated with a file called 'door-knock-3.wav'.
  PlaySoundEffect('knock');

  // Calls LinePrint ->
  // This is where we pass the procedure our parameters as defined above in the comments for 'LinePrint'.
  // First we print 'Knock knock...' with the procedures defined layout. E.g. text colour, location on the screen, font.
  // Then we print 'Who's there...' etc etc
  LinePrint('Knock knock...');
  LinePrint('Who''s there...?');
  LinePrint('Cows go...');
  LinePrint('Cows go who...?');
  // Calls PunchLine ->
  // 'PunchLine' behaves in the same way as 'LinePrint', additionally is plays the laughing sound.
  PunchLine('No, cows go Moo!');
end;

// This creates a new procedure called 'main', it has the program's
// `main' instructions.
procedure Main();
begin
  // Call the OpenAudio procedure -> effect is SplashKit can now play sound
  OpenAudio();

  // Call the OpenWindow procedure -> effect is a window appears
  //  'Knock Knock...', title to set for the window
  //  800, the width to set the window
  //  600, the height to set the window
  OpenWindow('Knock Knock...', 800, 600);

  // Call the LoadDefaultColors procedure -> effect is SplashKit colors are initialised


  // Call LoadResources -> effect is our images/sounds are loaded into SplashKit for use
  LoadResources();

  // Call DrawMySplashScreen -> effect is my custom splash screen is loaded.
  DrawMySplashScreen();

  // Call DrawDoor -> effect is the background door is shown... with delay for use to see it.
  DrawDoor();

  // Call ShowKnockKnock -> shows knock knock text and plays sound
  ShowKnockKnock();

  // Call CloseAudio -> turns off SplashKit's audio
  CloseAudio();
end;


// This is where the program starts to run...
begin
  // It calls the Main procedure, which contains the program's
  // 'main' instructions and data.
  Main();
end.
