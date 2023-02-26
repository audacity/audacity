/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectEditor.h

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from EffectPlugin.h

**********************************************************************/
#ifndef __AUDACITY_EFFECT_EDITOR__
#define __AUDACITY_EFFECT_EDITOR__

#include "Observer.h"
#include <wx/event.h>

class EffectSettingsAccess;
class EffectUIServices;
class wxWindow;

//! Message sent by EffectEditor when a setting is changed by the user
struct EffectSettingChanged final
{
   size_t index { size_t(-1) };
   float newValue {};
};

class WX_EFFECTS_API EffectEditor /* not final */
    : public Observer::Publisher<EffectSettingChanged>
{
public:
   EffectEditor(
      const EffectUIServices &services, EffectSettingsAccess &access);

   virtual ~EffectEditor();

   //! Get settings data from the panel; may make error dialogs and return false
   /*!
    @return true only if panel settings are acceptable
    */
   virtual bool ValidateUI() = 0;

   //! Update appearance of the panel for changes in settings
   /*!
    Default implementation does nothing, returns true

    @return true if successful
    */
   virtual bool UpdateUI();

   /*!
    Default implementation returns false
    @return true if using a native plug-in UI, not widgets
    */
   virtual bool IsGraphicalUI();

   //! On the first call only, may disconnect from further event handling
   /*!
    Default implemantation does nothing
    */
   virtual void Disconnect();

   /*!
    Handle the UI OnClose event.
    Default implementation calls mUIServices.CloseUI()
   */
   virtual void OnClose();

   //! Enable or disable the Apply button of the dialog that contains parent
   static bool EnableApply(wxWindow *parent, bool enable = true);

   // id that should be used by preview play button of effect dialog
   static constexpr int kPlayID = 20102;

   //! Enable or disable the preview play button of the dialog that contains
   //! parent
   static bool EnablePreview(wxWindow *parent, bool enable = true);

 protected:
   // Convenience function template for binding event handler functions
   template<typename EventTag, typename Class, typename Event>
   void BindTo(
      wxEvtHandler &src, const EventTag& eventType, void (Class::*pmf)(Event &))
   {
      src.Bind(eventType, pmf, static_cast<Class *>(this));
   }

   const EffectUIServices &mUIServices;
   EffectSettingsAccess &mAccess;

   bool mUIClosed { false };
};
#endif
