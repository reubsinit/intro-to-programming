# include <stdio.h>
# include <math.h>
# include <string.h>
# include "user_input.h"

# define DATA_SIZE 2

void swap(double* a, double* b)
{
	double temp = *b;

	*b = *a;
	*a = temp;
}

void sort(double data[], int size)
{
	int i;
	int j;

	for (i = 0; i < size; i ++)
	{
		for (j = 0; j <= (i - 1); j++)
		{
			if (data[j] > data[j + 1])
			{
				swap(&data[j], &data[j + 1]);
			}
		}
	}
}

double median(const double data[], int size)
{
	int temp = size;
	double result = (size % 2);
	if (result == 0)
	{
		temp = ((temp / 2) - 1);
		result = (data[temp] + data[temp + 1]) / 2;
	}
	else
	{
		temp = (int)((temp - 1) / 2);
		result = data[temp];
	}
	return result;
}

double sum(const double data[], int size)
{
	int i;
	double result = 0;

	for(i = 0; i < size; i++)
	{
		result += data[i];
	}
	return result;
}

double mean(const double data[], int size)
{
	return sum(data, size)/size;
}

double max(const double data[], int size)
{
	int i;
	double result = data[0];

	for(i = 0; i < size; i++)
	{
		if (result < data[i])
		{
			result = data[i];
		}	
	}
	return result;
}

void populate_array(double data[], int size)
{
	int i;
	char prompt[17] = "";
	char buffer[3] = "";

	for(i = 0; i < size; i++)
	{
		strncpy(prompt, "Enter value ", 13);
		sprintf(buffer, "%d", (i + 1) % 100);
		strncat(prompt, buffer, 2);
		strncat(prompt, ": ", 2);
		data[i] = read_double(prompt);
	}
}

void print_array(double data[], int size)
{
	int i;

	for(i = 0; i < size; i++)
	{
		printf("%d: %f.\n", (i + 1), data[i]);
	}
}

int main()
{
	double data[DATA_SIZE];

	populate_array(data, DATA_SIZE);
	printf("\nThis is the data you entered...\n");
	print_array(data, DATA_SIZE);
	printf("\nCalculating statistics....\n\n");
	printf("Sum:		%4.2f\n", sum(data, DATA_SIZE));
	printf("Mean:		%4.2f\n", mean(data, DATA_SIZE));
	printf("Max:		%4.2f\n", max(data, DATA_SIZE));
	printf("Median:		%4.2f\n", median(data, DATA_SIZE));
	sort(data, DATA_SIZE);
	printf("\nThis is the data sorted!\n");
	print_array(data, DATA_SIZE);

	return 0;
}


