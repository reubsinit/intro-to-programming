# include <stdio.h>
# include <math.h>
# include <unistd.h>
# include <string.h>
# include "user_input.h"
# define BOOK_SIZE 3

typedef struct target
{
	input_string name;
	input_string description;
	int bounty;
	float difficulty;
} target;

target read_target()
{
	target temp;
	target result;

	temp.name = read_string("\nPlease enter a name: ");
	temp.description = read_string("\nPlease enter a short description: ");
	temp.bounty = read_integer("\nPlease enter a bounty value: ");
	temp.difficulty = read_double("\nPlease enter a difficulty: ");

	result = temp;

	return result;
}

void populate_hit_list(target data[], int size)
{
	int i;

	for(i = 0; i < size; i++)
	{
		printf("For target %d...\n", i + 1);
		data[i] = read_target();
	}
}

void print_target(target targets[], int size)
{
	int i;

	for(i = 0; i < size; i++)
	{
		printf("%d: %s:- %s.\n $%d in bounty. Difficulty rating: %f\n", 
		(i + 1), 
		targets[i].name.str, 
		targets[i].description.str, 
		targets[i].bounty, 
		targets[i].difficulty);
	}
}


void save_address_book(target data[], int size)
{
	int i;
	FILE *out;

	out = fopen("targets.txt", "w");
	if (out == NULL) return;

	for(i = 0; i < size; i++)
	{
		fprintf(out, "%s\n%s\n%d\n%f\n", data[i].name.str, data[i].description.str, data[i].bounty, data[i].difficulty);
	}
	fclose(out);
}

void load_hit_list(target data[], int size)
{
	int i;
	FILE *input;

	input = fopen("targets.txt", "r");
	if (input == NULL) return;

	for(i = 0; i < size; i++)
	{
		fscanf(input, "%s %s %d ", data[i].name.str, data[i].description.str, &data[i].bounty, &data[i].difficulty);
	}
	fclose(input);
}

int main()
{
	target data[BOOK_SIZE];

	if (access("targets.txt", F_OK) != -1)
	{
		load_hit_list(data, BOOK_SIZE);
	}
	else
	{
		populate_hit_list(data, BOOK_SIZE);
	}

	print_target(data, BOOK_SIZE);
	save_address_book(data, BOOK_SIZE);
	return 0;
}



