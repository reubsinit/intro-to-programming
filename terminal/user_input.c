#include <stdio.h>
#include "user_input.h"

input_string read_string(const char* prompt)
{
  input_string result;    
  printf("%s", prompt); 
  scanf(" %255[^\n]", result.str ); 
  return result; 
 } 


int read_integer(const char* prompt)
{
  input_string line;
  int result;    
  char temp; 
  
  line = read_string(prompt);

  while ( sscanf(line.str, " %d %c", &result, &temp) != 1 )
  { 
    printf("Please enter a whole number.\n");
    line = read_string(prompt);
  }
  
  return result;
}

int read_integer_range(const char* prompt, int min, int max)
{
  int result;
  result = read_integer(prompt);
  while (result < min || result > max)
  {
    printf("Please enter a value between %d and %d.\n", min, max);
    result = read_integer(prompt);
  }
  return result;
}

float read_double(const char* prompt)
{
  input_string line;
  float result;
  char temp;

  line = read_string(prompt);

  while(sscanf(line.str, "%f %c", &result, &temp) !=1)
  {
    printf("Please enter a whole number.\n");
    line = read_string(prompt);
  }
  return result;
}

float read_double_range(const char* prompt, double min, double max)
{
  double result;
  result = read_double(prompt);
  while (result < min || result > max)
  {
    printf("Please enter a value between %f and %f.\n", min, max);
    result = read_double(prompt);
  }
  return result;
}