/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioUnitCocoaHelper.h

  Leland Lucius

**********************************************************************/

#ifndef AUDACITY_AUDIOUNIT_COCOA_HELPER_H
#define AUDACITY_AUDIOUNIT_COCOA_HELPER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <AudioUnit/AudioUnitCarbonView.h>

HIViewRef createGeneric(AudioUnit unit);
HIViewRef createPanner(AudioUnit unit);
HIViewRef createCocoa(AudioUnit unit);
HIViewRef createCarbon(AudioUnit unit, WindowRef window, AudioUnitCarbonView *carbonView);

#ifdef __cplusplus
}
#endif

#endif
