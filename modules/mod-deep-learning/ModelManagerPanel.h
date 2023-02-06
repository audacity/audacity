/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ModelManagerPanel.h
   Hugo Flores Garcia

******************************************************************/
/**

\file ModelManagerPanel.h
\brief ModelManagerPanel TODO

*/
/*******************************************************************/

#pragma once

#include "DeepModelManager.h"
#include "ModelCardPanel.h"
#include "Observer.h"

int getScreenWidth(float scale = 1);
int getScreenHeight(float scale = 1);
wxSize getManagerToolsPanelSize();

class ActiveModel;
class Effect;
class ShuttleGui;
class ModelManagerPanel;

static const int cardPanel_x_offset = 20;

static const int managerPanel_w = cardPanel_w + detailedCardPanel_h + 
                                   cardPanel_x_offset + 20;

class ManagerToolsPanel : public wxPanelWrapper
{
public:
   
   ManagerToolsPanel(wxWindow *parent, ModelManagerPanel *panel);

   void PopulateOrExchange(ShuttleGui & S);

   void SetFetchProgress(int64_t current, int64_t total);
   void OnAddRepo(wxCommandEvent & WXUNUSED(event));
   void OnExplore(wxCommandEvent & WXUNUSED(event));

private:
   wxButton *mAddRepoButton  { nullptr };
   wxButton *mExploreButton  { nullptr };
   ModelManagerPanel *mManagerPanel  { nullptr };

};

class ModelManagerPanel final : public wxPanelWrapper
{
   CardFetchedCallback GetCardFetchedCallback();
   CardFetchedCallback GetNewCardCallback();

public:
   ModelManagerPanel(wxWindow *parent, Effect *effect,
      std::shared_ptr<ActiveModel> pActiveModel, const std::string &deepEffectID);

   void PopulateOrExchange(ShuttleGui & S);

   void AddCard(ModelCardHolder card);
   void RemoveCard(ModelCardHolder card); // TODO

   void SetSelectedCard(ModelCardHolder card);

   void FetchCards();

private:
   Observer::Subscription mSubscription;

   wxScrolledWindow *mScroller  { nullptr };

   ManagerToolsPanel *mTools  { nullptr };
   std::map<std::string, std::unique_ptr<SimpleModelCardPanel>> mPanels;
   DetailedModelCardPanel *mDetailedPanel { nullptr };

   Effect *const mEffect;
   const std::shared_ptr<ActiveModel> mActiveModel;
   const std::string mDeepEffectID;
   
   friend class ManagerToolsPanel;
   friend class DeepLearningEffectBase;
};
