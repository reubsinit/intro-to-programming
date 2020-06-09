# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <unistd.h>
# include <string.h>
# include "user_input.h"
# define BOOK_SIZE 10

typedef struct contact
{
	input_string name;
	input_string phone;
	int age;
} contact;

typedef struct address_book
{
	contact *contacts;
	int num_contacts;
} address_book;

contact read_contact()
{
	contact temp;
	contact result;

	temp.name = read_string("\nPlease enter a name: ");
	temp.phone = read_string("\nPlease enter a phone number: ");
	temp.age = read_integer("\nPlease enter an age: ");

	result = temp;

	return result;
}

void populate_address_book(contact data[], int size)
{
	int i;

	for(i = 0; i < size; i++)
	{
		printf("For contact %d...\n", i + 1);
		data[i] = read_contact();
	}
}

void print_contact(contact data[], int size)
{
	int i;

	for(i = 0; i < size; i++)
	{
		printf("%d: %s:- %s. Aged %d\n", (i + 1), data[i].name.str, data[i].phone.str, data[i].age);
	}
}

void save_address_book(address_book book)
{
	int i;
	FILE *out;

	out = fopen("contacts.txt", "w");
	if (out == NULL) return;

	fprintf(out, "%d\n", book.num_contacts);

	for(i = 0; i < book.num_contacts; i++)
	{
		fprintf(out, "%s\n%s\n%d\n", book.contacts[i].name.str, book.contacts[i].phone.str, book.contacts[i].age);
	}
	fclose(out);
}

void load_address_book(address_book book)
{
	int i;
	FILE *input;

	input = fopen("contacts.txt", "r");
	if (input == NULL) return;

	book.num_contacts = 0;
	fscanf(input, " %d ", & book.num_contacts);

	book.contacts = (contact*) malloc(sizeof(contact) * book.num_contacts);

	for (i = 0; i < book.num_contacts; i++)
	{
		fscanf(input, "%s %s %d ", book.contacts[i].name.str, book.contacts[i].phone.str, & book.contacts[i].age);
	}

	fclose(input);
}

int main() /* Main Procedure */
{
	address_book friends;

	if (access("contacts.txt", F_OK) != -1) /* Check expression */
	{
		load_address_book(friends); /* If Expression True... */
	}
	else
	{
		friends.num_contacts = read_integer("Enter a number of contacts: ");
		friends.contacts = (contact*) malloc(sizeof(contact) * friends.num_contacts);

		populate_address_book(friends.contacts, friends.num_contacts);
	}

	print_contact(friends.contacts, friends.num_contacts);
	save_address_book(friends);

	return 0;
}



