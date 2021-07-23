/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file AccessibleLinksFormatter.h
 @brief Define a helper class to format text with link in a way, accessible to VI users.

 Dmitry Vedenko
 **********************************************************************/

#include "AccessibleLinksFormatter.h"

#include "ShuttleGui.h"

#include <unordered_map>
#include <algorithm>
#include <memory>

#include <wx/hyperlink.h>

#ifdef __WXGTK__
#include <wx/stattext.h>
#endif

namespace
{    
size_t OffsetPosition(size_t position, size_t length)
{
   if (position == wxString::npos)
      return wxString::npos;

   return position + length;
}
}

AccessibleLinksFormatter::AccessibleLinksFormatter(TranslatableString message)
    : mMessage(std::move(message))
{
}

AccessibleLinksFormatter& AccessibleLinksFormatter::FormatLink(
   wxString placeholder, TranslatableString value, std::string targetURL)
{
   mFormatArguments.push_back({ 
       std::move(placeholder), 
       std::move(value), 
       {}, 
       std::move(targetURL)
   });

   return *this;
}

AccessibleLinksFormatter& AccessibleLinksFormatter::FormatLink(
   wxString placeholder, TranslatableString value,
   LinkClickedHandler handler)
{
   mFormatArguments.push_back({ 
       std::move(placeholder), 
       std::move(value), 
       std::move(handler), 
       {} 
   });

   return *this;
}

void AccessibleLinksFormatter::Populate(ShuttleGui& S) const
{
   // Just add the text, if there are no links to format
   if (mFormatArguments.empty())
   {
      S.AddFixedText(mMessage);
      return;
   }

#ifdef __WXGTK__
   // Non empty label is required, as wxHyperlinkCtrl would assert otherwise
   std::unique_ptr<wxHyperlinkCtrl> tempHyperlink
       = std::make_unique<wxHyperlinkCtrl>(S.GetParent(), wxID_ANY, wxT("temp"), wxString());

   const wxColour hyperlinkColour = tempHyperlink->GetNormalColour();

   tempHyperlink.reset();
#endif

   wxString translated = mMessage.Translation();

   std::vector<ProcessedArgument> processedArguments =
      ProcessArguments(translated);

   if (processedArguments.empty())
   {
      S.AddFixedText(mMessage);
      return;
   }

   const int borderSize = S.GetBorder();

   S.StartHorizontalLay(wxEXPAND);
   {
      S.SetBorder(0);
      S.AddSpace(borderSize);

      S.StartWrapLay(wxEXPAND, 1);
      {
         size_t currentPosition = 0;

         for (const ProcessedArgument& processedArgument : processedArguments)
         {
            const FormatArgument* argument = processedArgument.Argument;

            // Add everything between currentPosition and PlaceholderPosition

            if (currentPosition != processedArgument.PlaceholderPosition)
            {
               const size_t substrLength =
                  processedArgument.PlaceholderPosition - currentPosition;

               S.Prop(0).AddFixedText(
                  Verbatim(translated.substr(currentPosition, substrLength)));
            }

            // Add hyperlink
#ifndef __WXGTK__
            wxHyperlinkCtrl* hyperlink = safenew wxHyperlinkCtrl(
               S.GetParent(), wxID_ANY, argument->Value.Translation(),
               argument->TargetURL);

            if (argument->Handler)
            {
               hyperlink->Bind(
                  wxEVT_HYPERLINK, [handler = argument->Handler](wxHyperlinkEvent& evt) { 
                    handler(); 
               });
            }

            S.AddWindow(hyperlink, wxALIGN_TOP | wxALIGN_LEFT);
#else
            wxStaticText* hyperlink = S.AddVariableText(argument->Value);

            hyperlink->SetFont(hyperlink->GetFont().Underlined());
            hyperlink->SetForegroundColour(hyperlinkColour);
            hyperlink->SetCursor(wxCURSOR_HAND);

            hyperlink->Bind(wxEVT_LEFT_UP, [handler = argument->Handler, url = argument->TargetURL](wxEvent&) {
                if (handler)
                    handler();
                else if (!url.empty())
                    wxLaunchDefaultBrowser(url);
            });
#endif
            // Update the currentPostion to the first symbol after the
            // Placeholder

            currentPosition = OffsetPosition(
               processedArgument.PlaceholderPosition,
               argument->Placeholder.Length());

            if (currentPosition >= translated.Length())
               break;
         }

         if (currentPosition < translated.Length())
            S.AddFixedText(Verbatim(translated.substr(currentPosition)));
      }
      S.EndWrapLay();
   }
   S.EndHorizontalLay();

   S.SetBorder(borderSize);
}

std::vector<AccessibleLinksFormatter::ProcessedArgument>
AccessibleLinksFormatter::ProcessArguments(wxString translatedMessage) const
{
   std::vector<ProcessedArgument> result;
   result.reserve(mFormatArguments.size());
   // Arguments with the same placeholder are processed left-to-right.
   // Lets track the last known position of the placeholder
   std::unordered_map<wxString, size_t> knownPlaceholderPosition;

   for (const FormatArgument& argument : mFormatArguments)
   {
      auto it = knownPlaceholderPosition.find(argument.Placeholder);

      const size_t startingPosition =
         it != knownPlaceholderPosition.end() ?
            OffsetPosition(it->second, argument.Placeholder.length()) :
            0;

      const size_t placeholderPosition =
         startingPosition == wxString::npos ?
            wxString::npos :
            translatedMessage.find(argument.Placeholder, startingPosition);

      knownPlaceholderPosition[argument.Placeholder] = placeholderPosition;

      if (placeholderPosition != wxString::npos)
      {
         result.emplace_back(
            ProcessedArgument { &argument, placeholderPosition });
      }
   }

   std::sort(
      result.begin(), result.end(),
      [](const ProcessedArgument& lhs, const ProcessedArgument& rhs) {
         return lhs.PlaceholderPosition < rhs.PlaceholderPosition;
      });

   return result;
}
