#ifndef _INSTR_H
#define _INSTR_H

#include "globals.h"

/* C interface to Instrmnt */

/* Instrument types */
#define CLARINET	0
#define SAXOFONY	1
#define BOWED           2
#define BANDEDWG        3
#define MANDOLIN        4
#define SITAR           5
#define MODALBAR        6
#define FLUTE           7



struct instr;

#ifdef __cplusplus
extern "C" {
#endif

struct stkgen *initStkGen();
int deleteStkGen(struct stkgen *);
MY_FLOAT gentick(struct stkgen *);

void setrawwavepath(char *);
struct instr *initInstrument(int instr_type, int sample_rate);

int deleteInstrument(struct instr* in);

int noteOn(struct instr* in, MY_FLOAT frequency, MY_FLOAT amplitude);

int noteOff(struct instr* in, MY_FLOAT amplitude);

int setFrequency(struct instr* in, MY_FLOAT frequency);

//MY_FLOAT lastOut(struct instr* in);

MY_FLOAT tick(struct instr* in);

//MY_FLOAT *multTicks(struct instr* in, MY_FLOAT *vector, unsigned int vectorSize);

int controlChange(struct instr* in, int number, MY_FLOAT value);

#ifdef __cplusplus
}
#endif

#endif

