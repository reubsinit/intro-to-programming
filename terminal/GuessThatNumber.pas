program GuessThatNumber;
uses SysUtils, Math, UserInput;

const
HARD_GUESS_COUNT = 2;
NORMAL_GUESS_COUNT = 5;
MIN = 1;
MAX = 100;

procedure GuessingGame(min, max: Integer; mode: Integer);
var
	answer, guess, guessCount: Integer;
begin
	Randomize();
	guessCount := 0;
	answer := Random(max) + 1;
	WriteLn();
	WriteLn('I''m thinking of a number between ', min, ' and ', max);
	WriteLn();
	repeat 
		guessCount := guessCount + 1;
		WriteLn('Guess # ', guessCount);
		guess := ReadIntegerRange('What do you think it is? ', min, max);
		if answer = guess then
		begin
			WriteLn('Good guess mother home_dawg!');
			WriteLn('Woohoooo!');
		end
		else if guess < answer then
		begin
			WriteLn('The number is larger than that.');
		end
		else
			WriteLn('The number is less than that');
		WriteLn();
	until (guessCount = ifthen(mode = HARD_GUESS_COUNT, HARD_GUESS_COUNT, NORMAL_GUESS_COUNT)) or (guess = answer);

	if (guessCount = ifthen(mode = HARD_GUESS_COUNT, HARD_GUESS_COUNT, NORMAL_GUESS_COUNT)) and not (guess = answer) then
	begin
		WriteLn('You ran out of guesses homie!');
		WriteLn('The answer was ', answer);
		WriteLn('Maybe Try again!');
	end;
end;

procedure Menu();
var
	option: Char;
begin
	Repeat
		WriteLn('Do you want to play?');
		WriteLn(' - [Y]es');
		WriteLn(' - [N]o');
		WriteLn(' - [H]ard Mode!');
		WriteLn(' - [M]aybe');
		WriteLn('Option -> ');
		ReadLn(option);

		case option of
			'y', 'Y': GuessingGame(MIN, MAX, NORMAL_GUESS_COUNT);
			'n', 'N': WriteLn('Bye!');
			'h', 'H': GuessingGame(MIN, MAX, HARD_GUESS_COUNT);
			'm', 'M': WriteLn('Hello?');
		else 
			WriteLn('Shiieeeet. Please try again.');
		end;
		WriteLn(); 
	until (option =  'n') or (option = 'N');
end;

procedure Main();
begin
	Menu();
end;

begin
	Main();
end.