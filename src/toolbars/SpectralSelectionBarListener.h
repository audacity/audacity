/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectralSelectionBarListener.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTRAL_SELECTION_BAR_LISTENER__
#define __AUDACITY_SPECTRAL_SELECTION_BAR_LISTENER__

#include "../Audacity.h"

class ComponentInterfaceSymbol;
using NumericFormatId = ComponentInterfaceSymbol;

class AUDACITY_DLL_API SpectralSelectionBarListener /* not final */ {

 public:

   SpectralSelectionBarListener(){}
   virtual ~SpectralSelectionBarListener(){}

   virtual double SSBL_GetRate() const = 0;

   virtual const NumericFormatId & SSBL_GetFrequencySelectionFormatName() = 0;
   virtual void SSBL_SetFrequencySelectionFormatName(const NumericFormatId & formatName) = 0;

   virtual const NumericFormatId & SSBL_GetBandwidthSelectionFormatName() = 0;
   virtual void SSBL_SetBandwidthSelectionFormatName(const NumericFormatId & formatName) = 0;

   virtual void SSBL_ModifySpectralSelection(double &bottom, double &top, bool done) = 0;
};

#endif
