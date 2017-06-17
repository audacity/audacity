/**********************************************************************

  Audacity: A Digital Audio Editor

  TimeToolbarListener.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_TIME_TOOLBAR_LISTENER__
#define __AUDACITY_TIME_TOOLBAR_LISTENER__

class SelectedRegion;

class AUDACITY_DLL_API TimeToolbarListener /* not final */ {

 public:

   TimeToolbarListener(){};
   virtual ~TimeToolbarListener(){};

   virtual double ATTB_GetRate() = 0;
   virtual void ATTB_SetRate(double rate) = 0;
   virtual const wxString & ATTB_GetSelectionFormat() = 0;
   virtual void ATTB_SetSelectionFormat(const wxString & format) = 0;
};

#endif
