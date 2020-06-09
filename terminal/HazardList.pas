program Hazards;

uses
	sysutils, UserInput;

type
	Hazard = record
		description: String;
		id: Integer;
		estCost: Double;
	end;

function CreateHazard(): Hazard;
begin
	result.description := ReadString('Enter your hazard description: ');
	result.id := ReadInteger('Enter the hazard ID: ');
	result.estCost := ReadDouble('Estimated cost of Hazard: ');
end;

procedure PrintHazard(hazards: array of Hazard);
var
	i: Integer;
begin
	for i := Low(hazards) to High(hazards) do
	begin
		WriteLn('Description: ', hazards[i].description, '.');
		WriteLn('ID: ', hazards[i].id, '.');
		WriteLn('Estimated cost: ', hazards[i].estCost:0:2, '.');
	end;
end;

procedure PopulateHazard(var hazards: array of Hazard);
var
	i: Integer;
begin
	for i := Low(hazards) to High(hazards) do
	begin
		hazards[i] := CreateHazard();
	end;
end;

procedure Main();
var
	hazards: array of Hazard;
	arrayLength: Integer;
begin
	arrayLength := ReadInteger('Hazards to log: ');
	SetLength(hazards, arrayLength);
	PopulateHazard(hazards);
	PrintHazard(hazards);
end;

begin
	Main();
end.