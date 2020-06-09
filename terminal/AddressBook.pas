program AddressBook;
uses sysutils, UserInput;

type
	ContactPointer = ^Contact;

	Contact = record
		name: String;
		phone: String;
		age: Integer;
		friend: ContactPointer;
	end;

function ReadContact(): Contact;
begin
    result.name := ReadString('Name: ');
    result.phone := ReadString('Phone: ');
    result.age := ReadInteger('Age: ');

	result.friend := nil;
end;

procedure PopulateAddressBook(var addressBook: array of contact);
var
	i: Integer;
begin
	for i := Low(addressBook) to High(addressBook) do
	begin
	WriteLn('For contact ', i + 1, ': ');
	addressBook[i] := ReadContact();
		
	end;
end;

procedure PrintContacts(const addressBook: array of Contact);
var
	i: Integer;
begin
	for i := Low(addressBook) to High(addressBook) do
	begin
		WriteLn(i, ': ', addressBook[i].name, ' ph: ', addressBook[i].phone, ' age: ', addressBook[i].age);
		if addressBook[i].friend <> nil then
			WriteLn('------ ', addressBook[i].friend^.name);
	end;
	
end;

procedure BecomeFriends(var addressBook: array of Contact);
var
	targetIdx, friendIdx: Integer;
begin
	PrintContacts(addressBook);
	targetIdx := ReadIntegerRange('Please enter the ID of your friend: ', 0, length(addressBook) - 1);
	friendIdx := ReadIntegerRange(Format('%s',['Please enter the ID to be ' + addressBook[targetIdx].name + '''s friend: ']), 0, length(addressBook) - 1);

	addressBook[targetIdx].friend := @addressBook[friendIdx];
	
end;

procedure Main();
var
	addressBook: array of Contact;
	count: Integer;
begin
	count := ReadInteger('How many friends do you have: ');
	SetLength(addressBook, Count);
	PopulateAddressBook(AddressBook);
	BecomeFriends(addressBook);
	PrintContacts(addressBook);	

end;

begin
	Main();
end.