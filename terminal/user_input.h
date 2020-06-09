#include <stdio.h>

typedef struct input_string
{
  char str[256]; 
} input_string;


input_string read_string(const char* prompt);

int read_integer(const char* prompt);

int read_integer_range(const char* prompt, int min, int max);

float read_double(const char* prompt);

float read_double_range(const char* prompt, double min, double max);