/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectralSelectionBarListener.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTRAL_SELECTION_BAR_LISTENER__
#define __AUDACITY_SPECTRAL_SELECTION_BAR_LISTENER__

#include "../Audacity.h"

class wxString;
class SelectedRegion;

class AUDACITY_DLL_API SpectralSelectionBarListener {

 public:

   SpectralSelectionBarListener(){};
   virtual ~SpectralSelectionBarListener(){};

   virtual double SSBL_GetRate() const = 0;

   virtual const wxString & SSBL_GetFrequencySelectionFormatName() = 0;
   virtual void SSBL_SetFrequencySelectionFormatName(const wxString & formatName) = 0;

   virtual const wxString & SSBL_GetLogFrequencySelectionFormatName() = 0;
   virtual void SSBL_SetLogFrequencySelectionFormatName(const wxString & formatName) = 0;

   virtual void SSBL_ModifySpectralSelection(double &bottom, double &top, bool done) = 0;
};

#endif
