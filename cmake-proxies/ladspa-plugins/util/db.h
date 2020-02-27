#ifndef _DB_H
#define _DB_H

#include "../ladspa-util.h"

void db_init();
static inline float f_lin2db_cube(float lin);
static inline float f_db2lin_cube(float db);
static inline float f_lin2db_lerp(float lin);
static inline float f_db2lin_lerp(float db);

extern float db_data[];
extern float lin_data[];

#define DB_TABLE_SIZE 1024
#define DB_MIN -60.0f
#define DB_MAX 24.0f
#define LIN_TABLE_SIZE 1024
#define LIN_MIN 0.0000000002f
#define LIN_MAX 9.0f

#ifdef DB_DEFAULT_CUBE
#define db2lin(a) f_db2lin_cube(a)
#define lin2db(a) f_lin2db_cube(a)
#else
#define db2lin(a) f_db2lin_lerp(a)
#define lin2db(a) f_lin2db_lerp(a)
#endif

static inline float f_db2lin_cube(float db)
{
	float scale = (db - DB_MIN) * (float)LIN_TABLE_SIZE / (DB_MAX - DB_MIN);
	int base = f_round(scale - 0.5f);
	float ofs = scale - base;

	if (base < 1) {
		return 0.0f;
	} else if (base > LIN_TABLE_SIZE - 3) {
		return lin_data[LIN_TABLE_SIZE - 2];
	}
	return cube_interp(ofs, lin_data[base-1], lin_data[base], lin_data[base+1], lin_data[base+2]);
}

static inline float f_db2lin_lerp(float db)
{
	float scale = (db - DB_MIN) * (float)LIN_TABLE_SIZE / (DB_MAX - DB_MIN);
	int base = f_round(scale - 0.5f);
	float ofs = scale - base;

	if (base < 1) {
		return 0.0f;
	} else if (base > LIN_TABLE_SIZE - 3) {
		return lin_data[LIN_TABLE_SIZE - 2];
	}
	return (1.0f - ofs) * lin_data[base] + ofs * lin_data[base+1];
}

static inline float f_lin2db_cube(float lin)
{
	float scale = (lin - LIN_MIN) * (float)DB_TABLE_SIZE / (LIN_MAX - LIN_MIN);
	int base = f_round(scale - 0.5f);
	float ofs = scale - base;

	if (base < 2) {
		return db_data[2] * scale * 0.5f - 23 * (2.0f - scale);
	} else if (base > DB_TABLE_SIZE - 3) {
		return db_data[DB_TABLE_SIZE - 2];
	}
	return cube_interp(ofs, db_data[base-1], db_data[base], db_data[base+1], db_data[base+2]);
}

static inline float f_lin2db_lerp(float lin)
{
	float scale = (lin - LIN_MIN) * (float)DB_TABLE_SIZE / (LIN_MAX - LIN_MIN);
	int base = f_round(scale - 0.5f);
	float ofs = scale - base;

	if (base < 2) {
		return db_data[2] * scale * 0.5f - 23.0f * (2.0f - scale);
	} else if (base > DB_TABLE_SIZE - 2) {
		return db_data[DB_TABLE_SIZE - 1];
	}
	return (1.0f - ofs) * db_data[base] + ofs * db_data[base+1];
}

#endif
