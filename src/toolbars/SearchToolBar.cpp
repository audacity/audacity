#include "SearchToolBar.h"

// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>

#include <wx/setup.h> // for wxUSE_* macros

#ifndef WX_PRECOMP
#include <wx/app.h>
#include <wx/sizer.h>
#include <wx/tooltip.h>
#endif

#include "AllThemeResources.h"
#include "CommandContext.h"
#include "CommandManager.h"
#include "Prefs.h"
#include "Project.h"
#include "UndoManager.h"


IMPLEMENT_CLASS(SearchToolBar, ToolBar);


wxBEGIN_EVENT_TABLE(SearchToolBar, ToolBar)
   EVT_SEARCH(wxID_ANY, SearchToolBar::OnSearch)
wxEND_EVENT_TABLE()


Identifier SearchToolBar::ID()
{
   return wxT("Find");
}

SearchToolBar::SearchToolBar(AudacityProject& project)
   : ToolBar(project, XO("Find"), ID())
{
}

SearchToolBar::~SearchToolBar()
{
}

void SearchToolBar::OnSearch(wxCommandEvent& event)
{
   wxString input = event.GetString().Strip(wxString::both);

   mInputField->Clear();

   if (input.IsEmpty())
   {
      return;
   }

   // We have to query it each time as there isn't a way to get
   // notified of e.g. new commands. But because the search
   // action is a "one-time operation" this should be fine
   CommandIDs cmdids;
   std::vector<NormalizedKeyString> keys;
   std::vector<NormalizedKeyString> default_keys;
   TranslatableStrings labels, categories, prefixes;

   CommandManager::Get(mProject).GetAllCommandData(
      cmdids, keys, default_keys, labels, categories, prefixes, true);

   int i = 0;
   bool found = false;

   for (const auto& it : labels)
   {
      if (it.StrippedTranslation().CompareTo(input, wxString::ignoreCase) == 0)
      {
         found = true;
         break;
      }
      i++;
   }

   wxString dialogTitle = _("Find");

   if (!found)
   {
      wxMessageBox(_("Command not found"), dialogTitle, wxOK | wxICON_WARNING);
      return;
   }

   CommandManager &commandManager = CommandManager::Get(mProject);
   CommandContext ctx(mProject);

   auto res = commandManager
      .HandleTextualCommand(cmdids[i], ctx, NoFlagsSpecified, false);

   if (CommandManager::TextualCommandResult::CommandNotFound == res)
   {
      wxMessageBox(_("Command not found"), dialogTitle, wxOK | wxICON_WARNING);
   }
   else if (CommandManager::TextualCommandResult::CommandFailure == res)
   {
      wxMessageBox(_("Command failed"), dialogTitle, wxOK | wxICON_WARNING);
   }
}

void SearchToolBar::OnFocus(wxFocusEvent& evt)
{
   wxEventType type = evt.GetEventType();

   if (type == wxEVT_KILL_FOCUS)
   {
      if (mFocused)
      {
         mFocused = false;
         mInputField->AutoComplete(wxArrayString());
      }
      return;
   }
   else if (mFocused || type != wxEVT_SET_FOCUS)
   {
      return;
   }

   // Get suggestion list every time when the object
   // is focused to always have an updated list

   mFocused = true;

   TranslatableStrings suggestions;
   std::vector<bool> vExcludeFromMacros;
   CommandManager::Get(mProject)
      .GetAllCommandLabels(suggestions, vExcludeFromMacros, true);

   wxArrayString translatedSuggestions;
   translatedSuggestions.Alloc(suggestions.size());

   for (const auto& it : suggestions)
   {
      translatedSuggestions.push_back(it.StrippedTranslation());
   }

   mInputField->AutoComplete(translatedSuggestions);
}


static wxString getDescriptiveText()
{
   return XO("Find").StrippedTranslation();
}


void SearchToolBar::Create(wxWindow* parent)
{
   ToolBar::Create(parent);
   UpdatePrefs();
}

void SearchToolBar::UpdatePrefs()
{
   mInputField->Clear();
   mInputField->SetDescriptiveText(getDescriptiveText());
   ToolBar::UpdatePrefs();
}

void SearchToolBar::Populate()
{
   SetBackgroundColour(theTheme.Colour(clrMedium));

   Add(mToolSizer = safenew wxBoxSizer(wxHORIZONTAL));

#ifdef EXPERIMENTAL_RIGHT_ALIGNED_TEXTBOXES
   long flags = wxTE_RIGHT;
#else
   long flags = wxTE_LEFT;
#endif

   wxSize textFieldSize(400, -1); // Some languages have long entries

   mInputField = safenew wxSearchCtrl(
      this, wxID_ANY, wxEmptyString, wxDefaultPosition, textFieldSize, flags);

   mInputField->SetDescriptiveText(getDescriptiveText());
   mInputField->ShowCancelButton(true);
   mInputField->Bind(wxEVT_SET_FOCUS, &SearchToolBar::OnFocus, this);
   mInputField->Bind(wxEVT_KILL_FOCUS, &SearchToolBar::OnFocus, this);

   mToolSizer->Add(mInputField, 0, wxUP | wxDOWN | wxLEFT, 2);

   Layout();
}

 static RegisteredToolbarFactory factory{
    [](AudacityProject& project) {
       return ToolBar::Holder{ safenew SearchToolBar{ project } }; }
 };

#include "ToolManager.h"

 namespace {
    AttachedToolBarMenuItem sAttachment{
       /* i18n-hint: Clicking this menu item shows the toolbar for editing */
       SearchToolBar::ID(), wxT("ShowSearchTB"), XXO("&Search Toolbar"),
       { Registry::OrderingHint::End, "ShowPlayMeterTB" }
    };
 }
