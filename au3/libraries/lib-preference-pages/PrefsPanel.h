/**********************************************************************

  Audacity: A Digital Audio Editor

  PrefsPanel.h

  Joshua Haberman

*******************************************************************//**

\class PrefsPanel
\brief Base class for a panel in the PrefsDialog.  Classes derived from
this class include BatchPrefs, DirectoriesPrefs, GUIPrefs, KeyConfigPrefs,
MousePrefs, QualityPrefs, SpectrumPrefs.

  The interface works like this: Each panel in the preferences dialog
  must derive from PrefsPanel. You must override Apply() with code
  to validate fields (returning false if any are bad), updating the
  global preferences object gPrefs, and instructing the applicable parts
  of the program to re-read the preference options.

  To actually add the new panel, edit the PrefsDialog constructor
  to append the panel to its list of panels.

*//*******************************************************************/

#ifndef __AUDACITY_PREFS_PANEL__
#define __AUDACITY_PREFS_PANEL__

#include <functional>
#include "wxPanelWrapper.h" // to inherit
#include "ComponentInterface.h"
#include "Registry.h"

/* A few constants for an attempt at semi-uniformity */
#define PREFS_FONT_SIZE     8

#define BUILTIN_PREFS_PANEL_PREFIX wxT("Built-in PrefsPanel: ")

/* these are spacing guidelines: ie. radio buttons should have a 5 pixel
 * border on each side */
#define RADIO_BUTTON_BORDER    5
#define TOP_LEVEL_BORDER       5
#define GENERIC_CONTROL_BORDER 5

class AudacityProject;
class ShuttleGui;

class PREFERENCE_PAGES_API PrefsPanel /* not final */ : public wxPanelWrapper, ComponentInterface
{
    struct PrefsItem;

public:
    // An array of PrefsNode specifies the tree of pages in pre-order traversal.
    struct PrefsNode {
        using Factory
            =std::function< PrefsPanel* (
                                wxWindow* parent, wxWindowID winid, AudacityProject*) >;
        Factory factory;
        size_t nChildren{ 0 };
        bool expanded{ false };
        mutable bool enabled{ true };

        PrefsNode(const Factory& factory_,
                  unsigned nChildren_ = 0,
                  bool expanded_ = true)
            : factory(factory_), nChildren(nChildren_), expanded(expanded_)
        {}
    };

    using Factories = std::vector<PrefsPanel::PrefsNode>;
    static Factories& DefaultFactories();

    // \brief Type alias for factories such as GUIPrefsFactory that produce a
    // PrefsPanel, used by the Preferences dialog in a treebook.
    // The project pointer may be null.  Usually it's not needed because
    // preferences are global.  But sometimes you need a project, such as to
    // preview the preference changes for spectrograms.
    using Factory
        =std::function< PrefsPanel* (
                            wxWindow* parent, wxWindowID winid, AudacityProject*) >;

    // Typically you make a static object of this type in the .cpp file that
    // also implements the PrefsPanel subclass.
    struct PREFERENCE_PAGES_API Registration final : Registry::RegisteredItem<PrefsItem>
    {
        Registration(const wxString& name, const Factory& factory, bool expanded = true,
                     const Registry::Placement& placement = { wxEmptyString, {} });
    };

    PrefsPanel(wxWindow* parent,
               wxWindowID winid, const TranslatableString& title)
        :  wxPanelWrapper(parent, winid)
    {
        SetLabel(title);   // Provide visual label
        SetName(title);    // Provide audible label
    }

    virtual ~PrefsPanel();

    // NEW virtuals
    virtual void Preview() {} // Make tentative changes
    virtual bool Commit() = 0; // used to be called "Apply"

    virtual PluginPath GetPath() const override;
    virtual VendorSymbol GetVendor() const override;
    virtual wxString GetVersion() const override;

    //virtual ComponentInterfaceSymbol GetSymbol();
    //virtual wxString GetDescription();

    // If it returns True, the Preview button is added below the panel
    // Default returns false
    virtual bool ShowsPreviewButton();
    virtual void PopulateOrExchange(ShuttleGui& WXUNUSED(S)) {}

    //! If not empty string, the Help button is added below the panel
    /*! Default returns empty string. */
    virtual ManualPageID HelpPageName();

    virtual void Cancel();

private:
    struct Traits : Registry::DefaultTraits {
        using NodeTypes = List<PrefsItem>;
    };
    struct PREFERENCE_PAGES_API PrefsItem final : Registry::GroupItem<Traits> {
        PrefsPanel::Factory factory;
        bool expanded{ false };

        static Registry::GroupItem<Traits>& Registry();

        PrefsItem(const wxString& name, const PrefsPanel::Factory& factory, bool expanded);

        struct Visitor;
    };
};

#endif
