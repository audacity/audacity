// interface for STK Effects

#include <stdlib.h>

#include "stkint.h"

#include "NRev.h"
#include "JCRev.h"
#include "PRCRev.h"

using namespace Nyq;

// Reverb Effects ==========================================================

struct stkEffect {
  Effect * effectPtr;
};

struct stkEffect *initStkEffect(int eff_type, ::StkFloat trev, int sample_rate) {
  struct stkEffect * eff = (struct stkEffect *) malloc(sizeof(struct stkEffect));
  Stk::setSampleRate(sample_rate);
  switch(eff_type) {
  case NREV:
    eff->effectPtr = new NRev(trev);
    break;
  case JCREV:
    eff->effectPtr = new JCRev(trev);
    break;
  case PRCREV:
    eff->effectPtr = new PRCRev(trev);
    break;
  default:
    return NULL;
  }
  return eff;
}

int deleteStkEffect(struct stkEffect * eff) {
  delete(eff->effectPtr);
  free(eff);
  return 0;
}

::StkFloat stkEffectTick(struct stkEffect * eff, ::StkFloat s) {
  return eff->effectPtr->tick(s);
}

void stkEffectSetMix (struct stkEffect * eff, ::StkFloat mix) {
  eff->effectPtr->setEffectMix(mix);
}

// PitShift ===================================================

#include "PitShift.h"

struct stkEffect *initStkPitShift(::StkFloat shift, int sample_rate) {
  PitShift * ps;
  ps = new PitShift();
  ps->setShift(shift);
  struct stkEffect * eff  = (struct stkEffect *) malloc(sizeof(struct stkEffect));
  Stk::setSampleRate(sample_rate);
  eff->effectPtr = ps;
  return eff;
} 

// Chorus =====================================================

#include "Chorus.h"

struct stkEffect *initStkChorus(::StkFloat baseDelay, ::StkFloat depth, ::StkFloat freq, int sample_rate) {
  Chorus * ch;
  ch = new Chorus(baseDelay);
  ch->setModDepth(depth);
  ch->setModFrequency(freq);
  struct stkEffect * eff  = (struct stkEffect *) malloc(sizeof(struct stkEffect));
  Stk::setSampleRate(sample_rate);
  eff->effectPtr = ch;
  return eff;
} 

