
/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ModelCardPanel.h
   Hugo Flores Garcia

******************************************************************/
/**

\file ModelCardPanel.h
\brief ModelCardPanel implements UI panels to display deep learning model cards.

*/
/*******************************************************************/

#pragma once

#include "DeepModelManager.h"
#include "ModelCard.h"
#include "wx/colour.h"
#include "wx/simplebook.h"

class ActiveModel;
class Effect;
class ShuttleGui;


//! \class ModelCardPanel 
//! \brief ModelCardPanel implements an abstract class for building UI panels 
//! to display deep learning model cards.
class ModelCardPanel /* not final */: public wxPanelWrapper
{
public:
   ModelCardPanel(wxWindow *parent, wxWindowID winid, 
                  ModelCardHolder card, Effect *effect,
                  std::shared_ptr<ActiveModel> pActiveModel,
                  const wxSize& size);

   virtual void PopulateOrExchange(ShuttleGui &S) = 0;
   bool TransferDataToWindow() override;
   bool TransferDataFromWindow() override;

   // calbacks
   void OnInstall(wxCommandEvent &event);
   void OnCancelInstall(wxCommandEvent &event);
   void OnUninstall(wxCommandEvent &event);

   void OnClick(wxMouseEvent &event);
   void OnApply(wxCommandEvent &event);
   void OnMoreInfo(wxCommandEvent &event);


   ModelCardHolder GetCard() const { return mCard; }


   enum class InstallStatus 
   {
      Uninstalled, 
      Installing, 
      Installed
   };

   enum class ModelStatus
   {
      Enabled,
      Disabled
   };

   void SetInstallStatus(InstallStatus status);
   void SetModelStatus(ModelStatus status);
   void PopulateWithNewCard(ModelCardHolder card);

protected:
   std::map<InstallStatus, wxColour> mInstallStatusColors = {
      { InstallStatus::Uninstalled, wxColour(207, 99, 119) },
      { InstallStatus::Installing,  wxColour(184, 122, 0) },
      { InstallStatus::Installed,   wxColour(42, 157, 143) }
   };

   using DomainTag = std::string;
   std::map<DomainTag, wxColour> mTagColors = {
      { "music",           wxColour(207, 99, 119) },
      { "speech",          wxColour(233, 196, 106) },
      { "environmental",   wxColour(42, 157, 143) },
      { "other",           wxColour(168, 218, 220) },
   };

protected:

   void Populate();
   void PopulateNameAndAuthor(ShuttleGui &S);
   void PopulateDomainTags(ShuttleGui &S);
   void PopulateShortDescription(ShuttleGui &S);
   void PopulateLongDescription(ShuttleGui &S);
   void PopulateMoreInfo(ShuttleGui &S);
   void PopulateMetadata(ShuttleGui &S);
   void PopulateInstallCtrls(ShuttleGui &S);

   void FetchModelSize();

private:
   wxWindow *mParent {nullptr};

   wxStaticText *mModelName {nullptr};
   wxStaticText *mModelSize {nullptr};
   wxStaticText *mModelAuthor {nullptr};
   wxStaticText *mShortDescription {nullptr};
   wxStaticText *mLongDescription {nullptr};

   wxSimplebook *mInstallStatusBook {nullptr};
   wxGauge *mProgressGauge {nullptr};
   std::map<InstallStatus, size_t> mInstallStatusPageMap;

   wxButton *mApplyButton {nullptr};
   wxButton *mMoreInfoButton {nullptr};

   ModelCardHolder mCard {nullptr};

   Effect *const mEffect;
   const std::shared_ptr<ActiveModel> mActiveModel {nullptr};
};

class SimpleModelCardPanel final : public ModelCardPanel
{
public:
   SimpleModelCardPanel(wxWindow *parent, wxWindowID id, 
                           ModelCardHolder card, Effect *effect,
                           std::shared_ptr<ActiveModel> pActiveModel);
   void PopulateOrExchange(ShuttleGui &S);
};

class DetailedModelCardPanel final : public ModelCardPanel
{
public:
   DetailedModelCardPanel(wxWindow *parent, wxWindowID id, 
                        ModelCardHolder card, Effect *effect,
                        std::shared_ptr<ActiveModel> pActiveModel);
   void PopulateOrExchange(ShuttleGui &S);
};

static const int cardPanel_w = 600;
static const int cardPanel_h = 150;

static const int detailedCardPanel_w = 400;
static const int detailedCardPanel_h = 400;
