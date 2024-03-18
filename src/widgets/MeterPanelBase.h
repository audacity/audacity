/**********************************************************************

Audacity: A Digital Audio Editor

MeterPanelBase.h

Paul Licameli split from MeterPanel.h

**********************************************************************/

#ifndef __AUDACITY_METER_PANEL_BASE__
#define __AUDACITY_METER_PANEL_BASE__

#include <utility>
#include "wxPanelWrapper.h"

class Meter;

//! Inherits wxPanel and has a Meter; exposes shared_ptr to the Meter.
/*! Derived classes supply implementations of its pure virtual functions,
 and the Meter object forwards calls to them.  This indirection is done so
 shared_ptr or weak_ptr to Meter may be used where such pointers to wxWindow
 classes don't work.
 */
class AUDACITY_DLL_API MeterPanelBase /* not final */
   : public wxPanelWrapper
{
public:
   using wxPanelWrapper::wxPanelWrapper;
   ~MeterPanelBase() override;

   template< typename ...Args >
      MeterPanelBase( Args &&...args )
         : wxPanelWrapper( std::forward<Args>(args)... )
      {}

   //! Expose a pointer to a Meter that can be tracked by std::weak_ptr
   std::shared_ptr<Meter> GetMeter() const;

private:
   static bool s_AcceptsFocus;
   struct Resetter { void operator () (bool *p) const { if(p) *p = false; } };
   using TempAllowFocus = std::unique_ptr<bool, Resetter>;

   bool AcceptsFocus() const override { return s_AcceptsFocus; }
   bool AcceptsFocusFromKeyboard() const override { return true; }

public:
   static TempAllowFocus TemporarilyAllowFocus();

protected:
   //! If the object can dynamic_cast to Meter, then the object returned by
   //! GetMeter will forward calls to it
   void Init(wxEvtHandler *pMeter);

   struct Forwarder;
   std::shared_ptr<Forwarder> mForwarder;
};

#endif
