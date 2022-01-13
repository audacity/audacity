/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ActiveModel.h
   Hugo Flores Garcia

******************************************************************/
/**

\file ActiveModel.h
\brief Store the user's choice of model, and send notifications of its changes

*/
/*******************************************************************/
#pragma once

#include <functional>
#include "DeepModel.h"
#include "ModelCard.h"
#include "Observer.h"

class Effect;

class ActiveModel : public Observer::Publisher<ModelCardHolder>
{
public:
   ActiveModel();

   const DeepModelHolder &GetModel() const { return mModel; }
   
   //! If any card was chosen, set model to that
   //! @param effect is only for issuing dialog boxes
   void SetModel(Effect &effect);
   //! Set the model to the given card, or reset if card is null
   //! @param effect is only for issuing dialog boxes
   void SetModel(Effect &effect, ModelCardHolder card);

private:
   //! the deep model itself
   DeepModelHolder mModel;
   ModelCardHolder mCard;
};
