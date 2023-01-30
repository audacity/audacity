/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file EffectPlugin.h

   Paul Licameli
   split from EffectInterface.h

**********************************************************************/

#ifndef __AUDACITY_EFFECTPLUGIN_H__
#define __AUDACITY_EFFECTPLUGIN_H__

#include "EffectInterface.h"

#include <functional>
#include <memory>

class EffectSettingsManager;

class wxDialog;
class wxWindow;
class EffectUIServices;
class EffectInstance;
class EffectSettings;
class EffectSettingsAccess;
class EffectPlugin;
class EffectEditor;

struct DialogFactoryResults {
   wxDialog *pDialog{};
   //! constructed and successfully Init()-ed; or null for failure
   std::shared_ptr<EffectInstance> pInstance{};
   EffectEditor *pEditor{};
};

//! Type of function that creates a dialog for an effect
/*! The dialog may be modal or non-modal */
using EffectDialogFactory = std::function< DialogFactoryResults(
   wxWindow &parent, EffectPlugin &, EffectUIServices &,
   EffectSettingsAccess &) >;

class TrackList;
class WaveTrackFactory;
class NotifyingSelectedRegion;
class EffectInstance;

/***************************************************************************//**
\class EffectPlugin
@brief Factory of instances of an effect and of dialogs to control them
*******************************************************************************/
class AUDACITY_DLL_API EffectPlugin
   : public EffectInstanceFactory
{
public:
   using EffectSettingsAccessPtr = std::shared_ptr<EffectSettingsAccess>;

   const static wxString kUserPresetIdent;
   const static wxString kFactoryPresetIdent;
   const static wxString kCurrentSettingsIdent;
   const static wxString kFactoryDefaultsIdent;

   EffectPlugin &operator=(EffectPlugin&) = delete;
   virtual ~EffectPlugin();

   //! Whether there are preferences applying to the effect's family, for which
   //! a UI may be needed to change them
   virtual bool HasOptions() const = 0;

   //! Whether the effect supports export of presets to files, and importing too
   virtual bool CanExportPresets() const = 0;

   virtual const EffectSettingsManager& GetDefinition() const = 0;

   //! Usually applies factory to self and given access
   /*!
    But there are a few unusual overrides for historical reasons

    @param pInstance may be passed to factory, and is only guaranteed to have
    lifetime suitable for a modal dialog, unless the dialog stores a copy of
    pInstance

    @param access is only guaranteed to have lifetime suitable for a modal
    dialog, unless the dialog stores access.shared_from_this()

    @return 0 if destructive effect processing should not proceed (and there
    may be a non-modal dialog still opened); otherwise, modal dialog return code
    */
   virtual int ShowHostInterface(
      wxWindow &parent, const EffectDialogFactory &factory,
      std::shared_ptr<EffectInstance> &pInstance, EffectSettingsAccess &access,
      bool forceModal = false) = 0;

   //! Returns the EffectUIServices instance for this effect
   /*!
    * Usually returns self. May return nullptr. EffectPlugin is responsible for the lifetime of the
    * returned instance.
    * @return EffectUIServices object or nullptr, if the effect does not implement the interface.
    */
   virtual EffectUIServices* GetEffectUIServices() = 0;

   //! Calculate temporary tracks of limited length with effect applied and play
   /*!
    @param updateUI called after adjusting temporary settings and before play
    */
   virtual void Preview(
      EffectSettingsAccess &access, std::function<void()> updateUI,
      bool dryOnly) = 0;
   virtual bool SaveSettingsAsString(
      const EffectSettings &settings, wxString & parms) const = 0;
   // @return nullptr for failure
   [[nodiscard]] virtual OptionalMessage LoadSettingsFromString(
      const wxString & parms, EffectSettings &settings) const = 0;
   virtual bool IsBatchProcessing() const = 0;
   virtual void SetBatchProcessing() = 0;
   virtual void UnsetBatchProcessing() = 0;

   //! Unfortunately complicated dual-use function
   /*!
    Sometimes this is invoked only to do effect processing, as a delegate for
    another effect, but sometimes also to put up a dialog prompting the user for
    settings first.

    Create a user interface only if the supplied factory is not null.
    Factory may be null because we "Repeat last effect" or apply a macro

    Will only operate on tracks that have the "selected" flag set to true,
    which is consistent with Audacity's standard UI.

    @return true on success
    */
   virtual bool DoEffect(
      EffectSettings &settings, //!< Always given; only for processing
      double projectRate, TrackList *list,
      WaveTrackFactory *factory, NotifyingSelectedRegion &selectedRegion,
      unsigned flags,
      // Prompt the user for input only if the next arguments are not all null.
      wxWindow *pParent = nullptr,
      const EffectDialogFactory &dialogFactory = {},
      const EffectSettingsAccessPtr &pAccess = nullptr
         //!< Sometimes given; only for UI
   ) = 0;

   //! Update controls for the settings
   virtual bool TransferDataToWindow(const EffectSettings &settings) = 0;

   //! Update the given settings from controls
   virtual bool TransferDataFromWindow(EffectSettings &settings) = 0;
};

/***************************************************************************//**
\class EffectInstanceEx
@brief Performs effect computation
*******************************************************************************/
class AUDACITY_DLL_API EffectInstanceEx : public virtual EffectInstance {
public:
   //! Call once to set up state for whole list of tracks to be processed
   /*!
    @return success
    Default implementation does nothing, returns true
    */
   virtual bool Init();

   //! Actually do the effect here.
   /*!
    @return success
    */
   virtual bool Process(EffectSettings &settings) = 0;

   ~EffectInstanceEx() override;
};

/*************************************************************************************//**

\class EffectUIServices

\brief EffectUIServices is an abstract base class to populate a UI and validate UI
values.  It can import and export presets.

*******************************************************************************************/
class AUDACITY_DLL_API EffectUIServices /* not final */
{
public:
   virtual ~EffectUIServices();

   /*!
    @return 0 if destructive effect processing should not proceed (and there
    may be a non-modal dialog still opened); otherwise, modal dialog return code
    */
   virtual int ShowClientInterface(const EffectPlugin &plugin,
      wxWindow &parent, wxDialog &dialog,
      EffectEditor *pEditor, bool forceModal = false) const = 0;

   //! Adds controls to a panel that is given as the parent window of `S`
   /*!
    @param S interface for adding controls to a panel in a dialog
    @param instance guaranteed to have a lifetime containing that of the returned
    object
    @param access guaranteed to have a lifetime containing that of the returned
    object
    @param pOutputs null, or else points to outputs with lifetime containing
    that of the returned object

    @return null for failure; else an object invoked to retrieve values of UI
    controls; it might also hold some state needed to implement event handlers
    of the controls; it will exist only while the dialog continues to exist
    */
   virtual std::unique_ptr<EffectEditor> PopulateUI(
      const EffectPlugin &plugin, ShuttleGui &S,
      EffectInstance &instance, EffectSettingsAccess &access,
      const EffectOutputs *pOutputs) const = 0;

   virtual void ExportPresets(
      const EffectPlugin &plugin, const EffectSettings &settings) const = 0;
   //! @return nullopt for failure
   [[nodiscard]] virtual OptionalMessage ImportPresets(
      const EffectPlugin &plugin, EffectSettings &settings) const = 0;

   virtual void ShowOptions(const EffectPlugin &plugin) const = 0;

   virtual bool ValidateUI(
      const EffectPlugin &context, EffectSettings &settings) const = 0;
   virtual bool CloseUI() const = 0;
};

/*************************************************************************************/ /**

 \class EffectSettingChanged

 \brief Message sent by EffectEditor when a setting is changed by the user

 *******************************************************************************************/
struct EffectSettingChanged final
{
   size_t index { size_t(-1) };
   float newValue {};
};

/*************************************************************************************//**

\class EffectEditor

\brief Interface for transferring values from a panel of effect controls

*******************************************************************************************/
class AUDACITY_DLL_API EffectEditor /* not final */
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
