/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file ListNavigationEnabled.h

   @author Vitaly Sverchinsky

**********************************************************************/

#pragma once

#include <wx/containr.h>

/**
 * \brief Changes default arrow navigation to behave more list- or table-like.
 * Instead of searching focusable items among children first, list navigation
 * searches for siblings when arrow key is pressed. Tab behaviour stays same.
 * Requires wxWANT_CHARS style flag to be set
 */
template<class WindowBase>
class ListNavigationEnabled : public wxNavigationEnabled<WindowBase>
{
public:
   ListNavigationEnabled()
   {
      WindowBase::Bind(wxEVT_NAVIGATION_KEY, &ListNavigationEnabled::OnNavigationKeyEvent, this);
      WindowBase::Bind(wxEVT_KEY_DOWN, &ListNavigationEnabled::OnKeyDown, this);
      WindowBase::Bind(wxEVT_CHAR_HOOK, &ListNavigationEnabled::OnCharHook, this);
   }

private:
   void SetFocus() override
   {
      //Prevent attempt to search for a focusable child
      WindowBase::SetFocus();
   }

   void OnCharHook(wxKeyEvent& evt)
   {
      //We want to restore focus to list item once arrow navigation is used
      //on the child item, for this we need a char hook since key/navigation
      //events are sent directly to the focused item
      const auto keyCode = evt.GetKeyCode();
      if((keyCode == WXK_DOWN || keyCode == WXK_UP) &&
         !WindowBase::HasFocus() &&
         WindowBase::IsDescendant(WindowBase::FindFocus()))
      {
         wxWindow::SetFocusFromKbd();
      }
      else
         evt.Skip();
   }
   
   void OnKeyDown(wxKeyEvent& evt)
   {
      const auto keyCode = evt.GetKeyCode();
      if(keyCode == WXK_TAB)
         WindowBase::NavigateIn(wxNavigationKeyEvent::FromTab | (evt.ShiftDown() ? wxNavigationKeyEvent::IsBackward : wxNavigationKeyEvent::IsForward));
      else if(keyCode == WXK_DOWN)
         WindowBase::Navigate(wxNavigationKeyEvent::IsForward);
      else if(keyCode == WXK_UP)
         WindowBase::Navigate(wxNavigationKeyEvent::IsBackward);
      else
         evt.Skip();
   }

   void OnNavigationKeyEvent(wxNavigationKeyEvent& evt)
   {
      if(evt.GetEventObject() == WindowBase::GetParent() && !evt.IsFromTab())
         WindowBase::SetFocusFromKbd();
      else if(evt.GetEventObject() == this && evt.GetCurrentFocus() == this && evt.IsFromTab())
      {
         //NavigateIn
         wxPropagationDisabler disableProp(evt);
         const auto isForward = evt.GetDirection();
         const auto& children = WindowBase::GetChildren();
         auto node = isForward ? children.GetFirst() : children.GetLast();
         while(node)
         {
            auto child = node->GetData();
            if(child->CanAcceptFocusFromKeyboard())
            {
               if(!child->GetEventHandler()->ProcessEvent(evt))
               {
                  child->SetFocusFromKbd();
               }
               evt.Skip(false);
               return;
            }
            node = isForward ? node->GetNext() : node->GetPrevious();
         }
      }
      else
         evt.Skip();
   }
   
   bool Destroy() override
   {
      if(WindowBase::IsDescendant(wxWindow::FindFocus()))
      {
         auto next = WindowBase::GetNextSibling();
         if(next != nullptr && next->AcceptsFocus())
            next->SetFocus();
         else
         {
            auto prev = WindowBase::GetPrevSibling();
            if(prev != nullptr && prev->AcceptsFocus())
               prev->SetFocus();
         }
      }
      return wxNavigationEnabled<WindowBase>::Destroy();
   }

};
