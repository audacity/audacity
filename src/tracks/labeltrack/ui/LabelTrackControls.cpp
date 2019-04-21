/**********************************************************************

Audacity: A Digital Audio Editor

LabelTrackControls.cpp

Paul Licameli split from TrackPanel.cpp

**********************************************************************/

#include "../../../Audacity.h"
#include "LabelTrackControls.h"

#include "../../../HitTestResult.h"
#include "../../../LabelTrack.h"
#include "../../../widgets/PopupMenuTable.h"
#include "../../../Prefs.h"
#include "../../../RefreshCode.h"
#include "../../../ShuttleGui.h"
#include "../../../widgets/wxPanelWrapper.h"
#include <wx/dialog.h>
#include <wx/fontenum.h>
#include <wx/listbox.h>
#include <wx/spinctrl.h>
#include "../../../Internat.h"

LabelTrackControls::~LabelTrackControls()
{
}

std::vector<UIHandlePtr> LabelTrackControls::HitTest
(const TrackPanelMouseState & state,
 const AudacityProject *pProject)
{
   return TrackControls::HitTest(state, pProject);
}

class LabelTrackMenuTable : public PopupMenuTable
{
   LabelTrackMenuTable() : mpData(NULL) {}
   DECLARE_POPUP_MENU(LabelTrackMenuTable);

public:
   static LabelTrackMenuTable &Instance();

   void InitMenu(Menu*, void *pUserData) override
   {
      mpData = static_cast<TrackControls::InitMenuData*>(pUserData);
   }

   void DestroyMenu() override
   {
      mpData = nullptr;
   }

   TrackControls::InitMenuData *mpData;

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
   POPUP_MENU_SEPARATOR()
   POPUP_MENU_ITEM(OnSetFontID, _("&Font..."), OnSetFont)
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
   facename = LabelTrack::GetFont(facename).GetFaceName();

   long fontsize = gPrefs->Read(wxT("/GUI/LabelFontSize"),
                                LabelTrack::DefaultFontSize);

   /* i18n-hint: (noun) This is the font for the label track.*/
   wxDialogWrapper dlg(mpData->pParent, wxID_ANY, wxString(_("Label Track Font")));
   dlg.SetName(dlg.GetTitle());
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
         S.AddPrompt(_("Face name"));
         lb = safenew wxListBox(&dlg, wxID_ANY,
            wxDefaultPosition,
            wxDefaultSize,
            facenames,
            wxLB_SINGLE);

         lb->SetName(_("Face name"));
         lb->SetSelection( make_iterator_range( facenames ).index( facename ));
         S.AddWindow(lb, wxALIGN_LEFT | wxEXPAND | wxALL);

         /* i18n-hint: (noun) The size of the typeface*/
         S.AddPrompt(_("Face size"));
         sc = safenew wxSpinCtrl(&dlg, wxID_ANY,
            wxString::Format(wxT("%ld"), fontsize),
            wxDefaultPosition,
            wxDefaultSize,
            wxSP_ARROW_KEYS,
            8, 48, fontsize);
         sc->SetName(_("Face size"));
         S.AddWindow(sc, wxALIGN_LEFT | wxALL);
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

   LabelTrack::ResetFont();

   mpData->result = RefreshCode::RefreshAll;
}

PopupMenuTable *LabelTrackControls::GetMenuExtension(Track *)
{
   return &LabelTrackMenuTable::Instance();
}
