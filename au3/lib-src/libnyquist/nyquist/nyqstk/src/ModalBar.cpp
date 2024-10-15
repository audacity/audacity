/***************************************************/
/*! \class ModalBar
    \brief STK resonant bar instrument class.

    This class implements a number of different
    struck bar instruments.  It inherits from the
    Modal class.

    Control Change Numbers: 
       - Stick Hardness = 2
       - Stick Position = 4
       - Vibrato Gain = 8
       - Vibrato Frequency = 11
       - Direct Stick Mix = 1
       - Volume = 128
       - Modal Presets = 16
         - Marimba = 0
         - Vibraphone = 1
         - Agogo = 2
         - Wood1 = 3
         - Reso = 4
         - Wood2 = 5
         - Beats = 6
         - Two Fixed = 7
         - Clump = 8

    by Perry R. Cook and Gary P. Scavone, 1995 - 2005.
*/
/***************************************************/

#include "ModalBar.h"
#include "SKINI.msg"
#include <cmath>

using namespace Nyq;

ModalBar :: ModalBar()
  : Modal()
{
  // Concatenate the STK rawwave path to the rawwave file
  wave_ = new FileWvIn( (Stk::rawwavePath() + "marmstk1.raw").c_str(), true );
  wave_->setRate( 0.5 * 22050.0 / Stk::sampleRate() );

  // Set the resonances for preset 0 (marimba).
  this->setPreset( 0 );
}

ModalBar :: ~ModalBar()
{
  delete wave_;
}

void ModalBar :: setStickHardness(StkFloat hardness)
{
  stickHardness_ = hardness;
  if ( hardness < 0.0 ) {
    errorString_ << "ModalBar::setStickHardness: parameter is less than zero ... setting to 0.0!";
    handleError( StkError::WARNING );
    stickHardness_ = 0.0;
  }
  else if ( hardness > 1.0 ) {
    errorString_ << "ModalBar::setStickHarness: parameter is greater than one ... setting to 1.0!";
    handleError( StkError::WARNING );
    stickHardness_ = 1.0;
  }

  wave_->setRate( (0.25 * pow(4.0, stickHardness_) ) );
  masterGain_ = 0.1 + (1.8 * stickHardness_);
}

void ModalBar :: setStrikePosition(StkFloat position)
{
  strikePosition_ = position;
  if ( position < 0.0 ) {
    errorString_ << "ModalBar::setStrikePosition: parameter is less than zero ... setting to 0.0!";
    handleError( StkError::WARNING );
    strikePosition_ = 0.0;
  }
  else if ( position > 1.0 ) {
    errorString_ << "ModalBar::setStrikePosition: parameter is greater than one ... setting to 1.0!";
    handleError( StkError::WARNING );
    strikePosition_ = 1.0;
  }

  // Hack only first three modes.
  StkFloat temp2 = position * PI;
  StkFloat temp = sin(temp2);                                       
  this->setModeGain(0, 0.12 * temp);

  temp = sin(0.05 + (3.9 * temp2));
  this->setModeGain(1, -0.03 * temp);

  temp = sin(-0.05 + (11 * temp2));
  this->setModeGain(2, 0.11 * temp);
}

void ModalBar :: setPreset(int preset)
{
  // Presets:
  //     First line:  relative modal frequencies (negative number is
  //                  a fixed mode that doesn't scale with frequency
  //     Second line: resonances of the modes
  //     Third line:  mode volumes
  //     Fourth line: stickHardness, strikePosition, and direct stick
  //                  gain (mixed directly into the output
  static StkFloat presets[9][4][4] = { 
    {{1.0, 3.99, 10.65, -2443},		// Marimba
     {0.9996, 0.9994, 0.9994, 0.999},
     {0.04, 0.01, 0.01, 0.008},
     {0.429688, 0.445312, 0.093750}},
    {{1.0, 2.01, 3.9, 14.37}, 		// Vibraphone
     {0.99995, 0.99991, 0.99992, 0.9999},	
     {0.025, 0.015, 0.015, 0.015 },
     {0.390625,0.570312,0.078125}},
    {{1.0, 4.08, 6.669, -3725.0},		// Agogo 
     {0.999, 0.999, 0.999, 0.999},	
     {0.06, 0.05, 0.03, 0.02},
     {0.609375,0.359375,0.140625}},
    {{1.0, 2.777, 7.378, 15.377},		// Wood1
     {0.996, 0.994, 0.994, 0.99},	
     {0.04, 0.01, 0.01, 0.008},
     {0.460938,0.375000,0.046875}},
    {{1.0, 2.777, 7.378, 15.377},		// Reso
     {0.99996, 0.99994, 0.99994, 0.9999},	
     {0.02, 0.005, 0.005, 0.004},
     {0.453125,0.250000,0.101562}},
    {{1.0, 1.777, 2.378, 3.377},		// Wood2
     {0.996, 0.994, 0.994, 0.99},	
     {0.04, 0.01, 0.01, 0.008},
     {0.312500,0.445312,0.109375}},
    {{1.0, 1.004, 1.013, 2.377},		// Beats
     {0.9999, 0.9999, 0.9999, 0.999},	
     {0.02, 0.005, 0.005, 0.004},
     {0.398438,0.296875,0.070312}},
    {{1.0, 4.0, -1320.0, -3960.0},		// 2Fix
     {0.9996, 0.999, 0.9994, 0.999},	
     {0.04, 0.01, 0.01, 0.008},
     {0.453125,0.453125,0.070312}},
    {{1.0, 1.217, 1.475, 1.729},		// Clump
     {0.999, 0.999, 0.999, 0.999},	
     {0.03, 0.03, 0.03, 0.03 },
     {0.390625,0.570312,0.078125}},
  };

  int temp = (preset % 9);
  for (unsigned int i=0; i<nModes_; i++) {
    this->setRatioAndRadius(i, presets[temp][0][i], presets[temp][1][i]);
    this->setModeGain(i, presets[temp][2][i]);
  }

  this->setStickHardness(presets[temp][3][0]);
  this->setStrikePosition(presets[temp][3][1]);
  directGain_ = presets[temp][3][2];

  if (temp == 1) // vibraphone
    vibratoGain_ = 0.2;
  else
    vibratoGain_ = 0.0;
}

void ModalBar :: controlChange(int number, StkFloat value)
{
  StkFloat norm = value * ONE_OVER_128;
  if ( norm < 0 ) {
    norm = 0.0;
    errorString_ << "ModalBar::controlChange: control value less than zero ... setting to zero!";
    handleError( StkError::WARNING );
  }
  else if ( norm > 1.0 ) {
    norm = 1.0;
    errorString_ << "ModalBar::controlChange: control value greater than 128.0 ... setting to 128.0!";
    handleError( StkError::WARNING );
  }

  if (number == __SK_StickHardness_) // 2
    this->setStickHardness( norm );
  else if (number == __SK_StrikePosition_) // 4
    this->setStrikePosition( norm );
  else if (number == __SK_ProphesyRibbon_) // 16
		this->setPreset((int) value);
  else if (number == __SK_Balance_) // 8
    vibratoGain_ = norm * 0.3;
  else if (number == __SK_ModWheel_) // 1
    directGain_ = norm;
  else if (number == __SK_ModFrequency_) // 11
    vibrato_.setFrequency( norm * 12.0 );
  else if (number == __SK_AfterTouch_Cont_)	// 128
    envelope_.setTarget( norm );
  else {
    errorString_ << "ModalBar::controlChange: undefined control number (" << number << ")!";
    handleError( StkError::WARNING );
  }

#if defined(_STK_DEBUG_)
    errorString_ << "ModalBar::controlChange: number = " << number << ", value = " << value << '.';
    handleError( StkError::DEBUG_WARNING );
#endif
}
