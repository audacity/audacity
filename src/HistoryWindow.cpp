/**********************************************************************

  Audacity: A Digital Audio Editor

  HistoryWindow.cpp

  Joshua Haberman
  Leland Lucius

*******************************************************************//*!

\class HistoryWindow
\brief Works with UndoManager to allow user to see descriptions of
and undo previous commands.  Also allows you to selectively clear the
undo memory so as to free up space.

*//*******************************************************************/

#include "Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/event.h>
#include <wx/imaglist.h>
#include <wx/intl.h>
#include <wx/listctrl.h>
#include <wx/settings.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "AudioIO.h"
#include "../images/Arrow.xpm"
#include "../images/Empty9x16.xpm"
#include "HistoryWindow.h"
#include "UndoManager.h"
#include "Project.h"
#include "ShuttleGui.h"
#include "Track.h"

enum {
   ID_AVAIL = 1000,
   ID_TOTAL,
   ID_LEVELS,
   ID_DISCARD
};

BEGIN_EVENT_TABLE(HistoryWindow, wxDialogWrapper)
   EVT_SIZE(HistoryWindow::OnSize)
   EVT_CLOSE(HistoryWindow::OnCloseWindow)
   EVT_LIST_ITEM_SELECTED(wxID_ANY, HistoryWindow::OnItemSelected)
   EVT_BUTTON(ID_DISCARD, HistoryWindow::OnDiscard)
END_EVENT_TABLE()

HistoryWindow::HistoryWindow(AudacityProject *parent, UndoManager *manager):
   wxDialogWrapper((wxWindow*)parent, wxID_ANY, wxString(_("Undo History")),
      wxDefaultPosition, wxDefaultSize,
      wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER )
{
   SetName(GetTitle());

   mManager = manager;
   mProject = parent;
   mSelected = 0;
   mAudioIOBusy = false;

   auto imageList = std::make_unique<wxImageList>(9, 16);
   imageList->Add(wxIcon(empty9x16_xpm));
   imageList->Add(wxIcon(arrow_xpm));

   //------------------------- Main section --------------------
   // Construct the GUI.
   ShuttleGui S(this, eIsCreating);

   S.SetBorder(5);
   S.StartVerticalLay(true);
   {
      S.StartStatic(_("&Manage History"), 1);
      {
         mList = S.AddListControlReportMode();
         // Do this BEFORE inserting the columns.  On the Mac at least, the
         // columns are deleted and later InsertItem()s will cause Audacity to crash.
         mList->SetSingleStyle(wxLC_SINGLE_SEL);
         mList->InsertColumn(0, _("Action"), wxLIST_FORMAT_LEFT, 300);
         mList->InsertColumn(1, _("Size"), wxLIST_FORMAT_LEFT, 85);

         //Assign rather than set the image list, so that it is deleted later.
         // AssignImageList takes ownership
         mList->AssignImageList(imageList.release(), wxIMAGE_LIST_SMALL);

         S.StartMultiColumn(3, wxCENTRE);
         {
            // FIXME: Textbox labels have inconsistent capitalization
            mTotal = S.Id(ID_TOTAL).AddTextBox(_("&Total space used"), wxT("0"), 10);
            mTotal->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(HistoryWindow::OnChar));
            S.AddVariableText(wxT(""))->Hide();

            mAvail = S.Id(ID_AVAIL).AddTextBox(_("&Undo Levels Available"), wxT("0"), 10);
            mAvail->Connect(wxEVT_KEY_DOWN, wxKeyEventHandler(HistoryWindow::OnChar));
            S.AddVariableText(wxT(""))->Hide();

            S.AddPrompt(_("&Levels To Discard"));
            mLevels = safenew wxSpinCtrl(this,
                                     ID_LEVELS,
                                     wxT("1"),
                                     wxDefaultPosition,
                                     wxDefaultSize,
                                     wxSP_ARROW_KEYS,
                                     0,
                                     mManager->GetCurrentState() - 1,
                                     0);
            S.AddWindow(mLevels);
            /* i18n-hint: (verb)*/
            mDiscard = S.Id(ID_DISCARD).AddButton(_("&Discard"));
         }
         S.EndMultiColumn();
      }
      S.EndStatic();

      S.StartHorizontalLay(wxALIGN_RIGHT, false);
      {
         S.SetBorder(10);
         S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();
   // ----------------------- End of main section --------------

   DoUpdate();
   mList->SetMinSize(mList->GetSize());
   Fit();
   SetMinSize(GetSize());
   mList->SetColumnWidth(0, mList->GetClientSize().x - mList->GetColumnWidth(1));
   mList->SetTextColour(wxSystemSettings::GetColour(wxSYS_COLOUR_WINDOWTEXT));

   wxTheApp->Connect(EVT_AUDIOIO_PLAYBACK,
                     wxCommandEventHandler(HistoryWindow::OnAudioIO),
                     NULL,
                     this);

   wxTheApp->Connect(EVT_AUDIOIO_CAPTURE,
                     wxCommandEventHandler(HistoryWindow::OnAudioIO),
                     NULL,
                     this);
}

HistoryWindow::~HistoryWindow()
{
   wxTheApp->Disconnect(EVT_AUDIOIO_PLAYBACK,
                        wxCommandEventHandler(HistoryWindow::OnAudioIO),
                        NULL,
                        this);

   wxTheApp->Disconnect(EVT_AUDIOIO_CAPTURE,
                        wxCommandEventHandler(HistoryWindow::OnAudioIO),
                        NULL,
                        this);

   mAvail->Disconnect(wxEVT_KEY_DOWN, wxKeyEventHandler(HistoryWindow::OnChar));
}

void HistoryWindow::OnAudioIO(wxCommandEvent& evt)
{
   evt.Skip();

   if (evt.GetInt() != 0)
      mAudioIOBusy = true;
   else
      mAudioIOBusy = false;

   mDiscard->Enable(!mAudioIOBusy);
}

void HistoryWindow::UpdateDisplay()
{
   if(IsShown())
      DoUpdate();
}

void HistoryWindow::DoUpdate()
{
   int i;

   mManager->CalculateSpaceUsage();

   mList->DeleteAllItems();

   wxLongLong_t total = 0;
   mSelected = mManager->GetCurrentState() - 1;
   for (i = 0; i < (int)mManager->GetNumStates(); i++) {
      wxString desc, size;

      total += mManager->GetLongDescription(i + 1, &desc, &size);
      mList->InsertItem(i, desc, i == mSelected ? 1 : 0);
      mList->SetItem(i, 1, size);
   }

   mTotal->SetValue(Internat::FormatSize(total));

   mList->EnsureVisible(mSelected);

   mList->SetItemState(mSelected,
                       wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                       wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);

   UpdateLevels();
}

void HistoryWindow::UpdateLevels()
{
   wxWindow *focus;
   int value = mLevels->GetValue();

   if (value > mSelected) {
      value = mSelected;
   }

   if (value == 0) {
      value = 1;
   }

   mLevels->SetValue(value);
   mLevels->SetRange(1, mSelected);

   mAvail->SetValue(wxString::Format(wxT("%d"), mSelected));

   focus = FindFocus();
   if ((focus == mDiscard || focus == mLevels) && mSelected == 0) {
      mList->SetFocus();
   }

   mLevels->Enable(mSelected > 0);
   mDiscard->Enable(mSelected > 0);
}

void HistoryWindow::OnDiscard(wxCommandEvent & WXUNUSED(event))
{
   int i = mLevels->GetValue();

   mSelected -= i;
   mManager->RemoveStates(i);
   mProject->SetStateTo(mSelected + 1);

   while(--i >= 0)
      mList->DeleteItem(i);

   DoUpdate();
}

void HistoryWindow::OnItemSelected(wxListEvent &event)
{
   if (mAudioIOBusy) {
      mList->SetItemState(mSelected,
                       wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED,
                       wxLIST_STATE_FOCUSED | wxLIST_STATE_SELECTED);
      return;
   }

   int selected = event.GetIndex();
   int i;

   for (i = 0; i < mList->GetItemCount(); i++) {
      mList->SetItemImage(i, 0);
      if (i > selected)
         mList->SetItemTextColour(i, *wxLIGHT_GREY);
      else
         mList->SetItemTextColour(i, mList->GetTextColour());
   }
   mList->SetItemImage(selected, 1);

   // Do not do a SetStateTo() if we're not actually changing the selected
   // entry.  Doing so can cause unnecessary delays upon initial load or while
   // clicking the same entry over and over.
   if (selected != mSelected) {
      mProject->SetStateTo(selected + 1);
   }
   mSelected = selected;

   UpdateLevels();
}

void HistoryWindow::OnCloseWindow(wxCloseEvent & WXUNUSED(event))
{
  this->Show(false);
}

void HistoryWindow::OnSize(wxSizeEvent & WXUNUSED(event))
{
   Layout();
   mList->SetColumnWidth(0, mList->GetClientSize().x - mList->GetColumnWidth(1));
   if (mList->GetItemCount() > 0)
      mList->EnsureVisible(mSelected);
}

void HistoryWindow::OnChar(wxKeyEvent &event)
{
   event.Skip(false);
   return;
}
