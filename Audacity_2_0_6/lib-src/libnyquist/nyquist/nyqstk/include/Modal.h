/***************************************************/
/*! \class Modal
    \brief STK resonance model instrument.

    This class contains an excitation wavetable,
    an envelope, an oscillator, and N resonances
    (non-sweeping BiQuad filters), where N is set
    during instantiation.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#ifndef STK_MODAL_H
#define STK_MODAL_H

#include "Instrmnt.h"
#include "Envelope.h"
#include "WaveLoop.h"
#include "SineWave.h"
#include "BiQuad.h"
#include "OnePole.h"

namespace Nyq
{

class Modal : public Instrmnt
{
public:
  //! Class constructor, taking the desired number of modes to create.
  /*!
    An StkError will be thrown if the rawwave path is incorrectly set.
  */
  Modal( unsigned int modes = 4 );

  //! Class destructor.
  virtual ~Modal();

  //! Reset and clear all internal state.
  void clear();

  //! Set instrument parameters for a particular frequency.
  virtual void setFrequency(StkFloat frequency);

  //! Set the ratio and radius for a specified mode filter.
  void setRatioAndRadius(unsigned int modeIndex, StkFloat ratio, StkFloat radius);

  //! Set the master gain.
  void setMasterGain(StkFloat aGain);

  //! Set the direct gain.
  void setDirectGain(StkFloat aGain);

  //! Set the gain for a specified mode filter.
  void setModeGain(unsigned int modeIndex, StkFloat gain);

  //! Initiate a strike with the given amplitude (0.0 - 1.0).
  virtual void strike(StkFloat amplitude);

  //! Damp modes with a given decay factor (0.0 - 1.0).
  void damp(StkFloat amplitude);

  //! Start a note with the given frequency and amplitude.
  void noteOn(StkFloat frequency, StkFloat amplitude);

  //! Stop a note with the given amplitude (speed of decay).
  void noteOff(StkFloat amplitude);

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
  virtual void controlChange(int number, StkFloat value) = 0;

protected:

  StkFloat computeSample( void );

  Envelope envelope_; 
  FileWvIn *wave_;
  BiQuad **filters_;
  OnePole  onepole_;
  SineWave vibrato_;

  unsigned int nModes_;
  std::vector<StkFloat> ratios_;
  std::vector<StkFloat> radii_;

  StkFloat vibratoGain_;
  StkFloat masterGain_;
  StkFloat directGain_;
  StkFloat stickHardness_;
  StkFloat strikePosition_;
  StkFloat baseFrequency_;
};

} // namespace Nyq

#endif
