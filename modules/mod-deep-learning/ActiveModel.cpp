/**********************************************************************
   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ActiveModel.cpp
   Hugo Flores Garcia
******************************************************************/
/*******************************************************************/

#include "ActiveModel.h"
#include "DeepModelManager.h"
#include "effects/Effect.h"

ActiveModel::ActiveModel()
{
   mModel = std::make_shared<DeepModel>();
}

void ActiveModel::SetModel(Effect &effect)
{
   if (mCard)
      SetModel(effect, mCard);
}

void ActiveModel::SetModel(Effect &effect, ModelCardHolder card)
{
   // if card is empty, reset the model
   if (!card)
   {
      mModel = std::make_shared<DeepModel>();
      mCard = nullptr;
   }
   else
   {
      auto &manager = DeepModelManager::Get();

      if (manager.IsInstalled(card))
      {
         // check if the current model is loaded (and that it's the one requested)
         bool ready = mModel->IsLoaded() && card->IsSame(*mModel->GetCard());
         if (!ready)
         {
            // attempt to load the model
            // if load fails, let the user know via a message box. 
            // make sure to propagate the exception's message to the message box. 
            try
            {
               mModel = manager.GetModel(card);
               mCard = card;
            }
            catch (const ModelException &e)
            {
               SetModel(effect, nullptr);
               effect.MessageBox(XO("An internal error occurred while loading the model. "
                        "Error message %s").Format(e.what()),
                        wxICON_ERROR);
            }
            catch (const ModelManagerException &e)
            {
               SetModel(effect, nullptr);
               effect.MessageBox(XO("An internal error occurred while loading the model. "
                        "Error message %s").Format(e.what()),
                        wxICON_ERROR);
            }
         }
      }
      else
      {
         // we're keeping the card even though it's not installed
         // so fill out with an empty model. 
         mModel = std::make_shared<DeepModel>();
         mModel->SetCard(card);
      }
   }

   Publish(card);
}
