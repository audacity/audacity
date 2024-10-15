/***************************************************/
/*! \class SineWave
    \brief STK sinusoid oscillator class.

    This class computes and saves a static sine "table" that can be
    shared by multiple instances.  It has an interface similar to the
    WaveLoop class but inherits from the Generator class.  Output
    values are computed using linear interpolation.

    The "table" length, set in SineWave.h, is 2048 samples by default.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "SineWave.h"
#include <cmath>

using namespace Nyq;

StkFrames SineWave :: table_;

SineWave :: SineWave( void )
  : time_(0.0), rate_(1.0), phaseOffset_(0.0)
{
  if ( table_.empty() ) {
    table_.resize( TABLE_SIZE + 1, 1 );
    StkFloat temp = 1.0 / TABLE_SIZE;
    for ( unsigned long i=0; i<=TABLE_SIZE; i++ )
      table_[i] = sin( TWO_PI * i * temp );
  }
}

SineWave :: ~SineWave()
{
}

void SineWave :: reset(void)
{
  time_ = 0.0;
  lastOutput_ = 0;
}

void SineWave :: setFrequency( StkFloat frequency )
{
  // This is a looping frequency.
  this->setRate( TABLE_SIZE * frequency / Stk::sampleRate() );
}

void SineWave :: addTime( StkFloat time )
{
  // Add an absolute time in samples.
  time_ += time;

  while ( time_ < 0.0 )
    time_ += TABLE_SIZE;
  while ( time_ >= TABLE_SIZE )
    time_ -= TABLE_SIZE;
}

void SineWave :: addPhase( StkFloat angle )
{
  // Add a time in cycles (one cycle = TABLE_SIZE).
  time_ += TABLE_SIZE * angle;

  while ( time_ < 0.0 )
    time_ += TABLE_SIZE;
  while ( time_ >= TABLE_SIZE )
    time_ -= TABLE_SIZE;
}

void SineWave :: addPhaseOffset( StkFloat angle )
{
  // Add a phase offset in cycles, where 1.0 = TABLE_SIZE.
  phaseOffset_ = TABLE_SIZE * angle;
}

StkFloat SineWave :: computeSample( void )
{
  // Check limits of time address ... if necessary, recalculate modulo
  // TABLE_SIZE.
  while ( time_ < 0.0 )
    time_ += TABLE_SIZE;
  while ( time_ >= TABLE_SIZE )
    time_ -= TABLE_SIZE;

  StkFloat tyme;
  if ( phaseOffset_ ) {
    tyme = time_ + phaseOffset_;
    while ( tyme < 0.0 )
      tyme += TABLE_SIZE;
    while ( tyme >= TABLE_SIZE )
      tyme -= TABLE_SIZE;
  }
  else {
    tyme = time_;
  }

  lastOutput_ = table_.interpolate( tyme );

  // Increment time, which can be negative.
  time_ += rate_;

  return lastOutput_;
}
