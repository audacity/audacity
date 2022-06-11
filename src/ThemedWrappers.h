/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file ThemedWrappers.h

   @author Vitaly Sverchinsky

   A set of classes which decorate given base classes with
   simplified theme support. Note that classes are declared
   final, which means they could be only used 'in-place'.
   
**********************************************************************/

#pragma once

#include <optional>

#include "Internat.h"
#include "Observer.h"
#include "Theme.h"
#include "Prefs.h"

template<class WindowBase>
class ThemedWindowWrapper final :
   public WindowBase,
   public PrefsListener
{
   Observer::Subscription mThemeChangeSubscription;
   std::optional<TranslatableString> mTranslatableLabel;
   int mForegroundColorIndex { -1 };
   int mBackgroundColorIndex { -1 };
public:
   template <typename... Args>
   ThemedWindowWrapper(Args&&... args) : WindowBase( std::forward<Args>(args)... )
   {
      mThemeChangeSubscription =
         theTheme.Subscribe(*this, &ThemedWindowWrapper::OnThemeChange);
   }

   void SetBackgroundColorIndex(int index)
   {
      mBackgroundColorIndex = index;
      if(index != -1)
         WindowBase::SetBackgroundColour(theTheme.Colour(mBackgroundColorIndex));
   }

   void SetForegroundColorIndex(int index)
   {
      mForegroundColorIndex = index;
      if(index != -1)
         WindowBase::SetForegroundColour(theTheme.Colour(mForegroundColorIndex));
   }

   void SetTranslatableLabel(TranslatableString label)
   {
      mTranslatableLabel = std::move(label);
      WindowBase::SetLabel(mTranslatableLabel->Translation());
   }

   void UpdatePrefs() override
   {
      PrefsListener::UpdatePrefs();
      if(mTranslatableLabel)
         WindowBase::SetLabel(mTranslatableLabel->Translation());
   }

protected:
   void OnThemeChange(ThemeChangeMessage message)
   {
      if (message.appearance)
      {
         if(mBackgroundColorIndex != -1)
            WindowBase::SetBackgroundColour(theTheme.Colour(mBackgroundColorIndex));
         if(mForegroundColorIndex != -1)
            WindowBase::SetForegroundColour(theTheme.Colour(mForegroundColorIndex));
         WindowBase::Refresh();
      }
   }
};

template<class ButtonBase>
class ThemedButtonWrapper final :
   public ButtonBase,
   public PrefsListener
{
   Observer::Subscription mThemeChangeSubscription;
   int mBitmapIndex { -1 };
   int mLabelBitmapIndex { -1 };
   int mPressedBitmapIndex { -1 };
   int mCurrentBitmapIndex { -1 };
   int mBackgroundColorIndex { -1 };
   std::optional<TranslatableString> mTranslatableLabel;

public:
   template <typename... Args>
   ThemedButtonWrapper(Args&&... args) : ButtonBase( std::forward<Args>(args)... )
   {
      mThemeChangeSubscription =
         theTheme.Subscribe(*this, &ThemedButtonWrapper::OnThemeChange);
   }

   int GetBitmapIndex() const
   {
      return mBitmapIndex;
   }

   void SetBitmapIndex(int index)
   {
      mBitmapIndex = index;
      ButtonBase::SetBitmap(theTheme.Bitmap(mBitmapIndex));
   }

   void SetBitmapLabelIndex(int index)
   {
      mLabelBitmapIndex = index;
      ButtonBase::SetBitmapLabel(theTheme.Bitmap(mLabelBitmapIndex));
   }
   void SetBitmapPressedIndex(int index)
   {
      mPressedBitmapIndex = index;
      ButtonBase::SetBitmapPressed(theTheme.Bitmap(mPressedBitmapIndex));
   }

   void SetBitmapCurrentIndex(int index)
   {
      mCurrentBitmapIndex = index;
      ButtonBase::SetBitmapCurrent(theTheme.Bitmap(mCurrentBitmapIndex));
   }

   void SetBackgroundColorIndex(int index)
   {
      mBackgroundColorIndex = index;
      if(index != -1)
         ButtonBase::SetBackgroundColour(theTheme.Colour(mBackgroundColorIndex));
   }

   void SetTranslatableLabel(TranslatableString label)
   {
      mTranslatableLabel = std::move(label);
      ButtonBase::SetLabel(mTranslatableLabel->Translation());
   }

   void UpdatePrefs() override
   {
      PrefsListener::UpdatePrefs();
      if(mTranslatableLabel)
         ButtonBase::SetLabel(mTranslatableLabel->Translation());
   }

protected:
   void OnThemeChange(ThemeChangeMessage message)
   {
      if (message.appearance)
      {
         if(mBitmapIndex != -1)
            ButtonBase::SetBitmap(theTheme.Bitmap(mBitmapIndex));
         if(mLabelBitmapIndex != -1)
            ButtonBase::SetBitmapLabel(theTheme.Bitmap(mLabelBitmapIndex));
         if(mPressedBitmapIndex != -1)
            ButtonBase::SetBitmapPressed(theTheme.Bitmap(mPressedBitmapIndex));
         if(mCurrentBitmapIndex != -1)
            ButtonBase::SetBitmapCurrent(theTheme.Bitmap(mCurrentBitmapIndex));

         if(mBackgroundColorIndex != -1)
            ButtonBase::SetBackgroundColour(theTheme.Colour(mBackgroundColorIndex));
         ButtonBase::Refresh();
      }
   }
};
