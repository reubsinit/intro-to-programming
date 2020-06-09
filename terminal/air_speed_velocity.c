#include <stdio.h>
#include "user_input.h"

#define STROHAUL_NUMBER 0.33

double air_speed(float frequency, float amplitude)
{
	float result = (frequency * amplitude) / STROHAUL_NUMBER;
	return result;
}

int main()
{
	double input_amplitude, input_frequency, calc_amplitude, air_speed_velocity;
	input_string name;

	printf("Welcome to A Bird's Air Speed Velocity calculator.\n");
	input_frequency = read_double("At what Frequency (hz^2) do your bird's wings beat? ");
	input_amplitude = read_double("What is the amplitude of the stroke (cm^3)? ");
	calc_amplitude = (input_amplitude / 100);
	air_speed_velocity = air_speed(input_frequency, input_amplitude);
	printf("Okay, your bird's air speed velocity is %4.2f meters per second.\n", air_speed_velocity);


	return 0;
}


