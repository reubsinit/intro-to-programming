program AirSpeedVelocity;
uses UserInput;

const
STROHAUL_NUMBER = 0.33;

function AirSpeed(frequency, amplitude: Double): Double;
begin
	result := Round((frequency * amplitude) / STROHAUL_NUMBER);
end;

procedure Main();

var
inputAmplitude, inputFrequency, calcAmplitude, airSpeedVelocity: Double;

begin
	WriteLn('Welcome to A Bird''s Air Speed Veolcity calculator');
	inputFrequency := ReadDouble('At what Frequency (hz^2) do your bird''s wings beat? ');
	inputAmplitude := ReadDouble('What is the amplitude of the stroke (cm^3)? ');

	calcAmplitude := (inputAmplitude / 100);

	airSpeedVelocity := AirSpeed(inputFrequency, calcAmplitude);

	WriteLn('Okay, your bird''s airspeed velocity is ', Round(airSpeedVelocity), ' meters per second.');
end;

begin
	Main();
end.