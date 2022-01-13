/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ModelCardPanel.cpp
   Hugo Flores Garcia

******************************************************************/

#include "ModelCardPanel.h"

#include "effects/Effect.h"

#include "ActiveModel.h"
#include "Shuttle.h"
#include "ShuttleGui.h"

#include <wx/scrolwin.h>
#include <wx/range.h>
#include <wx/gauge.h>
#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/textdlg.h>
#include <wx/stattext.h>
#include <wx/hyperlink.h>

#include "Internat.h"
#include "AllThemeResources.h"
#include "Theme.h"

// DomainTagPanel

class DomainTagPanel : public wxPanelWrapper
{
public:
   DomainTagPanel(wxWindow *parent, const wxString &tag, const wxColour &color)
                  :wxPanelWrapper(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize)
   {
      auto name = Verbatim(tag);
      SetLabel(name);

      // SetWindowStyle(wxBORDER_SIMPLE);
      SetBackgroundColour(color);

      ShuttleGui S(this, eIsCreating);
      wxStaticText *txt = S.AddVariableText(name, true);
      SetMaxSize(wxSize(90, static_cast<int>(txt->GetSize().GetHeight() * 2)));
      SetVirtualSize(txt->GetSize());

      txt->SetBackgroundColour(color);
      wxFont font = txt->GetFont();
      font.SetPointSize(11);
      txt->SetFont(font);

      Refresh();
      
      Fit();
      Layout();
   }
};

// ModelCardPanel

ModelCardPanel::ModelCardPanel(wxWindow *parent, wxWindowID winid, ModelCardHolder card, Effect *effect,
   std::shared_ptr<ActiveModel> pActiveModel, const wxSize& size)
    : wxPanelWrapper(parent, winid, wxDefaultPosition, size, wxBORDER_SIMPLE | wxEXPAND),
      mParent{ parent },
      mCard { card },
      mEffect{ effect },
      mActiveModel{ move(pActiveModel) }
{
   SetLabel(XO("Model Card"));
   SetName(XO("Model Card"));

   Connect(
      winid,
      wxEVT_LEFT_DOWN,
      wxMouseEventHandler(ModelCardPanel::OnClick),
      NULL,
      this
   );
   // TransferDataToWindow();
}

void ModelCardPanel::Populate()
{
   SetAutoLayout(true);
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   Fit();
   Center();
   Layout();
}

void ModelCardPanel::PopulateWithNewCard(ModelCardHolder card)
{
   DestroyChildren();
   SetSizer(nullptr);

   mCard = card;
   Populate();
   Refresh();
   mParent->Fit();
   mParent->Refresh();
   mParent->Layout();
   mParent->GetParent()->Fit();
   mParent->GetParent()->Refresh();
   mParent->GetParent()->Layout();

}

bool ModelCardPanel::TransferDataToWindow()
{
   return true;
}

bool ModelCardPanel::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   gPrefs->Flush();

   return true;
}

void ModelCardPanel::PopulateNameAndAuthor(ShuttleGui &S)
{
   // {repo-name} [model size]
   // S.StartHorizontalLay(wxALIGN_LEFT, true);
   S.StartMultiColumn(2, wxALIGN_LEFT);
   {
      mModelName = S.AddVariableText(Verbatim(mCard->name()),
                                       false, wxLEFT);
      mModelName->SetFont(wxFont(wxFontInfo().Bold()));

      // model size
      float sizeMB = static_cast<float>(mCard->model_size()) / static_cast<float>(1024 * 1024);

      // i8n-hint: Model file size, in megabytes 
      mModelSize = S.AddVariableText(XO("[%.1f MB]").Format(sizeMB));
   }
   S.EndMultiColumn();
   // S.EndHorizontalLay();

   // by {author}
   S.StartHorizontalLay(wxALIGN_LEFT, true);
   {
      // i8n-hint: "by" here means "programmed by" and refers to the author
      // that developed the neural network model mentioned here. 
      S.AddVariableText(XC("by", "author of the model"));
      mModelAuthor = S.AddVariableText(Verbatim(mCard->author()));
      mModelAuthor->SetFont(wxFont(wxFontInfo().Bold()));
   }
   S.EndHorizontalLay();
}

void ModelCardPanel::PopulateDomainTags(ShuttleGui &S)
{
   S.StartHorizontalLay(wxALIGN_LEFT | wxALIGN_TOP, true);
   {
      static const int MAX_TAGS = 3;

      auto &tags = mCard->domain_tags();

      int numTags = tags.size() > MAX_TAGS ? MAX_TAGS : tags.size();
      for (int i = 0; i < numTags ; i++)
      {
         S.AddWindow(
            safenew DomainTagPanel(this, tags[i], mTagColors[tags[i]])
         );
      }
   }
   S.EndHorizontalLay();
}

void ModelCardPanel::PopulateShortDescription(ShuttleGui &S)
{
   // model description
   // S.StartStatic(XO("Description"));
   S.SetBorder(10);
   mShortDescription = S.AddVariableText(Verbatim(mCard->short_description()),
                                         false, wxLEFT);
   // S.EndStatic();
}

void ModelCardPanel::PopulateLongDescription(ShuttleGui &S)
{
   // model description
   S.StartStatic(Verbatim(""));
   mLongDescription = S.AddVariableText(
                                       Verbatim(mCard->long_description()),
                                       false, wxLEFT, 
                                       GetSize().GetWidth() - 30);
   S.EndStatic();
}

void ModelCardPanel::PopulateMetadata(ShuttleGui &S)
{
   S.StartMultiColumn(2, wxALIGN_LEFT);
   {
      S.AddVariableText(XO("Effect: "))
          ->SetFont(wxFont(wxFontInfo().Bold()));
      S.AddVariableText(Verbatim(mCard->effect_type()));

      S.AddVariableText(XO("Sample Rate: "))
          ->SetFont(wxFont(wxFontInfo().Bold()));
      S.AddVariableText(Verbatim("%d")
                            .Format(mCard->sample_rate()));

      std::string tagString;
      for (auto &tag : mCard->tags())
      {
         if (!tagString.empty())
            tagString = tagString +  ", " + tag;
         else
            tagString = tag;
      }
      
      S.AddVariableText(XO("Tags: "))
          ->SetFont(wxFont(wxFontInfo().Bold()));
      S.AddVariableText(Verbatim(tagString));
   }
   S.EndMultiColumn();
}

void ModelCardPanel::PopulateInstallCtrls(ShuttleGui &S)
{
   DeepModelManager &manager = DeepModelManager::Get();
 
   mInstallStatusBook = S.StartSimplebook();
   {
      // Installed
      mInstallStatusPageMap[InstallStatus::Installed] = 0;
      S.StartNotebookPage({});
      {
         auto button = S.AddButton(XO("Uninstall"));
         button->Connect(wxEVT_COMMAND_BUTTON_CLICKED, 
                           wxCommandEventHandler(ModelCardPanel::OnUninstall), NULL, this);
         auto statusText = S.AddVariableText(XO("installed"), true);

         wxColour statusColor = mInstallStatusColors[InstallStatus::Installed];
         statusText->SetForegroundColour(statusColor);
      }
      S.EndNotebookPage();
      
      // Uninstalled
      mInstallStatusPageMap[InstallStatus::Uninstalled] = 1;
      S.StartNotebookPage({});
      {
         auto button = S.AddButton(XO("Install"));
         button->Connect(wxEVT_COMMAND_BUTTON_CLICKED, 
                           wxCommandEventHandler(ModelCardPanel::OnInstall), NULL, this);
         auto statusText = S.AddVariableText(XO("uninstalled"), true);

         wxColour statusColor = mInstallStatusColors[InstallStatus::Uninstalled];
         statusText->SetForegroundColour(statusColor);
      }
      S.EndNotebookPage();

      // installing
      mInstallStatusPageMap[InstallStatus::Installing] = 2;
      S.StartNotebookPage({});
      {
         // show progress
         // add a progress gauge for downloads, but don't show it unless ur installing
         mProgressGauge = safenew wxGauge(S.GetParent(), wxID_ANY, 100); // TODO:  sizing
         mProgressGauge->SetSize(wxSize(80, 20));
         S.AddWindow(mProgressGauge);

         auto button = S.AddButton(XC("Cancel", "install"));
         button->Connect(wxEVT_COMMAND_BUTTON_CLICKED, 
                           wxCommandEventHandler(ModelCardPanel::OnCancelInstall), NULL, this);
         auto statusText = S.AddVariableText(XO("installing..."), true);

         wxColour statusColor = mInstallStatusColors[InstallStatus::Installing];
         statusText->SetForegroundColour(statusColor);
      }
      S.EndNotebookPage();
   }
   S.EndSimplebook();

   // we shouldn't be initializing GUI if an install is in progress?
   SetInstallStatus(
      manager.IsInstalled(mCard) ? InstallStatus::Installed : InstallStatus::Uninstalled
   );
}

void ModelCardPanel::PopulateMoreInfo(ShuttleGui &S)
{
   S.StartHorizontalLay(wxCENTER, true);
   {
      mMoreInfoButton = S.AddButton(XC("More Info", "model"));
      mMoreInfoButton->Connect(wxEVT_COMMAND_BUTTON_CLICKED, 
                           wxCommandEventHandler(ModelCardPanel::OnMoreInfo), NULL, this);
   }
   S.EndHorizontalLay();
}

void ModelCardPanel::SetInstallStatus(InstallStatus status)
{
   mInstallStatusBook->ChangeSelection(
      mInstallStatusPageMap[status]
   );
   this->Layout();
   this->GetParent()->Layout();
}

void ModelCardPanel::OnUninstall(wxCommandEvent &event)
{
   DeepModelManager &manager = DeepModelManager::Get();

   // TODO: show a prompt to confirm?
   manager.Uninstall(mCard);
   this->SetInstallStatus(InstallStatus::Uninstalled);

   mActiveModel->SetModel(*mEffect, nullptr);      
}

void ModelCardPanel::OnCancelInstall(wxCommandEvent &event)
{
   DeepModelManager &manager = DeepModelManager::Get();

   manager.CancelInstall(mCard);
   SetInstallStatus(InstallStatus::Uninstalled);
}

// TODO: this is good, but still hangs on "installing"
// even when you turn off the connection
void ModelCardPanel::OnInstall(wxCommandEvent &event)
{
   DeepModelManager &manager = DeepModelManager::Get();

   auto wthis = wxWeakRef(this);
   ProgressCallback onProgress([wthis, &manager](int64_t current, int64_t expected)
   {
      // bail if we've been deleted
      if (!wthis)
         return;

      // bail if the progress gauge isn't up yet
      if (!wthis->mProgressGauge)
         return;

      if (expected > 0)
      {
         // update the progress gauge
         wthis->mProgressGauge->SetRange(expected);
         wthis->mProgressGauge->SetValue(current);
      }
      else
         wthis->mProgressGauge->Pulse();
   });

   InstallCompletionHandler onInstallDone([wthis, &manager]()
   {  
      // bail if we've been deleted
      if (!wthis)
         return;

      // check if install succeeded
      if (manager.IsInstalled(wthis->mCard))
         wthis->SetInstallStatus(InstallStatus::Installed);
      else
         wthis->SetInstallStatus(InstallStatus::Uninstalled);
   });

   if (!manager.IsInstalled(mCard))
   {
      this->SetInstallStatus(InstallStatus::Installing);
      manager.Install(mCard, std::move(onProgress), std::move(onInstallDone));
   }
}

void ModelCardPanel::OnClick(wxMouseEvent &event)
{  
   mActiveModel->SetModel(*mEffect, mCard);
}

void ModelCardPanel::OnMoreInfo(wxCommandEvent &event)
{
   DeepModelManager &manager = DeepModelManager::Get();
   wxString url = wxString(manager.GetMoreInfoURL(mCard));
   wxLaunchDefaultBrowser(url);
}

void ModelCardPanel::SetModelStatus(ModelStatus status)
{
   if (status == ModelStatus::Enabled)
   {
      SetBackgroundColour(
         theTheme.Colour(clrMediumSelected)
      );
   }
   else
   {
      SetBackgroundColour(
         GetParent()->GetBackgroundColour()
      );
   }

   // Fit();
   Refresh();
}

// SimpleModelCardPanel

SimpleModelCardPanel::SimpleModelCardPanel(wxWindow *parent, wxWindowID id,
   ModelCardHolder card, Effect *effect,
   std::shared_ptr<ActiveModel> pActiveModel)
      : ModelCardPanel(parent, id, card, effect, move(pActiveModel),
         wxSize(cardPanel_w, cardPanel_h))
{
   Populate();
}


void SimpleModelCardPanel::PopulateOrExchange(ShuttleGui &S)
{
   // the layout is actually 2 columns,
   // but we add a small space in the middle, which takes up a column
   S.StartMultiColumn(3, wxEXPAND);
   {
      // left column:
      // repo name, repo author, model tags, and model description
      S.SetStretchyCol(0);
      S.StartVerticalLay(wxALIGN_LEFT, true);
      {
         PopulateNameAndAuthor(S);
         PopulateDomainTags(S);
         PopulateShortDescription(S);
      }
      S.EndVerticalLay();

      // dead space (center column)
      S.AddSpace(5, 0);

      S.StartMultiColumn(1);
      {
         // bottom: install and uninstall controls
         S.StartVerticalLay(wxALIGN_BOTTOM, false);
         {
            PopulateInstallCtrls(S);
         }
         S.EndVerticalLay();
      }
      S.EndVerticalLay();
   }
   S.EndMultiColumn();
}

// DetailedModelCardPanel

DetailedModelCardPanel::DetailedModelCardPanel(wxWindow *parent, wxWindowID id,
   ModelCardHolder card, Effect *effect,
   std::shared_ptr<ActiveModel> pActiveModel)
      : ModelCardPanel(parent, id, card, effect, move(pActiveModel),
                       wxSize(detailedCardPanel_w,
                              detailedCardPanel_h))
{
   if (card)
      Populate();
}

void DetailedModelCardPanel::PopulateOrExchange(ShuttleGui &S)
{
   S.StartVerticalLay(wxALIGN_LEFT, true);
   {
      PopulateNameAndAuthor(S);
      PopulateDomainTags(S);
      PopulateLongDescription(S);
      PopulateMetadata(S);
      PopulateMoreInfo(S);
   }
}
