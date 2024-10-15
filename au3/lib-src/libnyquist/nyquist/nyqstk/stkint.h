typedef double StkFloat;

#define NREV 0
#define JCREV 1
#define PRCREV 2

#ifdef __cplusplus
extern "C" {
#endif

  struct stkEffect;
  struct stkEffect * initStkEffect (int, StkFloat, int);
  int deleteStkEffect(struct stkEffect *);
  StkFloat  stkEffectTick(struct stkEffect *, StkFloat);
  void stkEffectSetMix (struct stkEffect *, StkFloat);

  struct stkEffect * initStkPitShift(StkFloat, int);
  struct stkEffect * initStkChorus(StkFloat, StkFloat, StkFloat, int);
  
#ifdef __cplusplus
}
#endif
