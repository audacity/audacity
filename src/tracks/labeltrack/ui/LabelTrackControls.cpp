/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/


#include "LabelTrackControls.h"

#include "LabelTrackView.h"
#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "../../../widgets/PopupMenuTable.h"
#include "Prefs.h"
#include "../../../RefreshCode.h"
#include "../../../ShuttleGui.h"
#include "../../../widgets/wxPanelWrapper.h"
#include <wx/dialog.h>
#include <wx/fontenum.h>
#include <wx/listbox.h>
#include <wx/spinctrl.h>

LabelTrackControls::~LabelTrackControls()
{
}

std::vector<UIHandlePtr> LabelTrackControls::HitTest
(const TrackPanelMouseState & state,
 const AudacityProject *pProject)
{
   return CommonTrackControls::HitTest(state, pProject);
}

class LabelTrackMenuTable : public PopupMenuTable
{
   LabelTrackMenuTable()
      : PopupMenuTable{ "LabelTrack" }
   {}
   DECLARE_POPUP_MENU(LabelTrackMenuTable);

public:
   static LabelTrackMenuTable &Instance();

   void InitUserData(void *pUserData) override
   {
      mpData = static_cast<CommonTrackControls::InitMenuData*>(pUserData);
   }

   CommonTrackControls::InitMenuData *mpData{};

   void OnSetFont(wxCommandEvent &);
};

LabelTrackMenuTable &LabelTrackMenuTable::Instance()
{
   static LabelTrackMenuTable instance;
   return instance;
}

enum
{
   OnSetFontID = 30000,
};

BEGIN_POPUP_MENU(LabelTrackMenuTable)
   BeginSection( "Appearance" );
      AppendItem( "Font", OnSetFontID, XXO("&Font..."), POPUP_MENU_FN( OnSetFont ) );
      BeginSection("Extra");
      EndSection();
   EndSection();
END_POPUP_MENU()

void LabelTrackMenuTable::OnSetFont(wxCommandEvent &)
{
   // Small helper class to enumerate all fonts in the system
   // We use this because the default implementation of
   // wxFontEnumerator::GetFacenames() has changed between wx2.6 and 2.8
   class FontEnumerator : public wxFontEnumerator
   {
   public:
      explicit FontEnumerator(wxArrayString* fontNames) :
         mFontNames(fontNames) {}

      bool OnFacename(const wxString& font) override
      {
         mFontNames->push_back(font);
         return true;
      }

   private:
      wxArrayString* mFontNames;
   };

   wxArrayString facenames;
   FontEnumerator fontEnumerator(&facenames);
   fontEnumerator.EnumerateFacenames(wxFONTENCODING_SYSTEM, false);

   wxString facename = gPrefs->Read(wxT("/GUI/LabelFontFacename"), wxT(""));

   // Correct for empty facename, or bad preference file:
   // get the name of a really existing font, to highlight by default
   // in the list box
   facename = LabelTrackView::GetFont(facename).GetFaceName();

   long fontsize = gPrefs->Read(wxT("/GUI/LabelFontSize"),
                                LabelTrackView::DefaultFontSize);

   /* i18n-hint: (noun) This is the font for the label track.*/
   wxDialogWrapper dlg(mpData->pParent, wxID_ANY, XO("Label Track Font"));
   dlg.SetName();
   ShuttleGui S(&dlg, eIsCreating);
   wxListBox *lb;
   wxSpinCtrl *sc;

   S.StartVerticalLay(true);
   {
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.SetStretchyRow(0);
         S.SetStretchyCol(1);

         /* i18n-hint: (noun) The name of the typeface*/
         S.AddPrompt(XXO("Face name"));
         lb = safenew wxListBox(S.GetParent(), wxID_ANY,
            wxDefaultPosition,
            wxDefaultSize,
            facenames,
            wxLB_SINGLE);

         lb->SetSelection( make_iterator_range( facenames ).index( facename ));
         S
            .Name(XO("Face name"))
            .Position(  wxALIGN_LEFT | wxEXPAND | wxALL )
            .AddWindow(lb);

         /* i18n-hint: (noun) The size of the typeface*/
         S.AddPrompt(XXO("Face size"));
         sc = safenew wxSpinCtrl(S.GetParent(), wxID_ANY,
            wxString::Format(wxT("%ld"), fontsize),
            wxDefaultPosition,
            wxDefaultSize,
            wxSP_ARROW_KEYS,
            8, 48, fontsize);
         S
            .Name(XO("Face size"))
            .Position( wxALIGN_LEFT | wxALL )
            .AddWindow(sc);
      }
      S.EndMultiColumn();
      S.AddStandardButtons();
   }
   S.EndVerticalLay();

   dlg.Layout();
   dlg.Fit();
   dlg.CenterOnParent();
   if (dlg.ShowModal() == wxID_CANCEL)
      return;

   gPrefs->Write(wxT("/GUI/LabelFontFacename"), lb->GetStringSelection());
   gPrefs->Write(wxT("/GUI/LabelFontSize"), sc->GetValue());
   gPrefs->Flush();

   LabelTrackView::ResetFont();

   mpData->result = RefreshCode::RefreshAll;
}

PopupMenuTable *LabelTrackControls::GetMenuExtension(Track *)
{
   return &LabelTrackMenuTable::Instance();
}

using DoGetLabelTrackControls = DoGetControls::Override< LabelTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(DoGetLabelTrackControls) {
   return [](LabelTrack &track) {
      return std::make_shared<LabelTrackControls>( track.SharedPointer() );
   };
}

using GetDefaultLabelTrackHeight = GetDefaultTrackHeight::Override< LabelTrack >;
DEFINE_ATTACHED_VIRTUAL_OVERRIDE(GetDefaultLabelTrackHeight) {
   return [](LabelTrack &) {
      // Label tracks are narrow
      // Default is to allow two rows so that NEW users get the
      // idea that labels can 'stack' when they would overlap.
      return 73;
   };
}

// The following attaches the label color sub-menu to the label track popup
// menu.  It is appropriate only to label view and so is kept in this
// source file with the rest of the label view implementation.

#include <mutex> // for std::call_once
#include "LabelTrackControls.h"
#include "../../../widgets/PopupMenuTable.h"
#include "../../../ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../../RefreshCode.h"

LabelTrackMenuTable& GetLabelTrackMenuTable()
{
   return LabelTrackMenuTable::Instance();
}

//=============================================================================
// Table class for a sub-menu

enum {
   OnInstrument1ID = OnSetFontID + 50,
   OnInstrument2ID,
   OnInstrument3ID,
   OnInstrument4ID,
};
struct LabelColorMenuTable : PopupMenuTable
{
   LabelColorMenuTable() : PopupMenuTable{ "LabelColor", XO("&Label Color") } {}
   DECLARE_POPUP_MENU(LabelColorMenuTable);

   static LabelColorMenuTable& Instance();

   void InitUserData(void* pUserData) override;

   CommonTrackControls::InitMenuData* mpData{};

   int IdOfLabelColor(int LabelColor);
   void OnLabelColorChange(wxCommandEvent& event);
};

LabelColorMenuTable& LabelColorMenuTable::Instance()
{
   static LabelColorMenuTable instance;
   return instance;
}

void LabelColorMenuTable::InitUserData(void* pUserData)
{
   mpData = static_cast<CommonTrackControls::InitMenuData*>(pUserData);
}

namespace {
   using ValueFinder = std::function< int(LabelTrack&) >;

   const TranslatableString GetLabelColorStr(int colorIndex)
   {
      return XXO("Instrument %i").Format(colorIndex + 1);
   }
}

BEGIN_POPUP_MENU(LabelColorMenuTable)
static const auto fn = [](PopupMenuHandler& handler, wxMenu& menu, int id) {
   auto& me = static_cast<LabelColorMenuTable&>(handler);
   auto pData = me.mpData;
   const auto& track = *static_cast<LabelTrack*>(pData->pTrack);
   auto& project = pData->project;
   bool unsafe = ProjectAudioIO::Get(project).IsAudioActive();

   menu.Check(id, id == me.IdOfLabelColor(track.GetColourIndex()));
   menu.Enable(id, !unsafe);
};

static std::once_flag flag;
std::call_once(flag, [this] {
   auto& hostTable = GetLabelTrackMenuTable();

   });

AppendRadioItem("Instrument1", OnInstrument1ID,
   GetLabelColorStr(0), POPUP_MENU_FN(OnLabelColorChange), fn);
AppendRadioItem("Instrument2", OnInstrument2ID,
   GetLabelColorStr(1), POPUP_MENU_FN(OnLabelColorChange), fn);
AppendRadioItem("Instrument3", OnInstrument3ID,
   GetLabelColorStr(2), POPUP_MENU_FN(OnLabelColorChange), fn);
AppendRadioItem("Instrument4", OnInstrument4ID,
   GetLabelColorStr(3), POPUP_MENU_FN(OnLabelColorChange), fn);

END_POPUP_MENU()

/// Converts a LabelColor enumeration to a wxWidgets menu item Id.
int LabelColorMenuTable::IdOfLabelColor(int LabelColor)
{
   return OnInstrument1ID + LabelColor;
}

/// Handles the selection from the LabelColor submenu of the
/// track menu.
void LabelColorMenuTable::OnLabelColorChange(wxCommandEvent& event)
{
   int id = event.GetId();
   wxASSERT(id >= OnInstrument1ID && id <= OnInstrument4ID);
   const auto pTrack = static_cast<LabelTrack*>(mpData->pTrack);

   int newLabelColor = id - OnInstrument1ID;

   AudacityProject* const project = &mpData->project;

   for (auto channel : TrackList::Channels(pTrack))
      channel->SetColourIndex(newLabelColor);

   ProjectHistory::Get(*project)
      .PushState(XO("Changed '%s' to %s")
         .Format(pTrack->GetName(), GetLabelColorStr(newLabelColor)),
         XO("LabelColor Change"));

   using namespace RefreshCode;
   mpData->result = RefreshAll | FixScrollbars;
}

namespace {
   PopupMenuTable::AttachedItem sAttachment{
      GetLabelTrackMenuTable(),
      { "Appearance/Extra" },
      std::make_unique<PopupMenuSection>("LabelColor",
      // Conditionally add sub-menu for label color, if showing labelform
      PopupMenuTable::Computed< LabelTrackMenuTable >(
         [](LabelTrackMenuTable& table) -> Registry::BaseItemPtr {
            return Registry::Shared(LabelColorMenuTable::Instance()
               .Get(table.mpData));
         }))
   };
}
