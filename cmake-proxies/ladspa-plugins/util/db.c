#include <stdio.h>
#include <math.h>

#include "db.h"

float db_data[DB_TABLE_SIZE];
float lin_data[LIN_TABLE_SIZE];

void db_init()
{
	unsigned int i;

	for (i=0; i<LIN_TABLE_SIZE; i++) {
		lin_data[i] = powf(10.0f, ((DB_MAX - DB_MIN) *
		   	(float)i/(float)LIN_TABLE_SIZE + DB_MIN) / 20.0f);
	}

	for (i=0; i<DB_TABLE_SIZE; i++) {
		db_data[i] = 20.0f * log10f((LIN_MAX - LIN_MIN) *
			(float)i/(float)DB_TABLE_SIZE + LIN_MIN);
	}
}
