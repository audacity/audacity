/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file AccessibleLinksFormatter.h
 @brief Define a helper class to format text with link in a way, accessible to VI users.

 Dmitry Vedenko
 **********************************************************************/

#pragma once

#include <functional>
#include <vector>

#include "TranslatableString.h"

class ShuttleGui;

/*! @brief A class that allows translatable text to have accessible links in it in a way
*          that is friendly to translators.
* 
* This class allows to replace arbitrary placeholders (like %s, %url, {} or anyting of the choice)
* with links, that are accessible from the keyboard. 
* 
* In case there are multiple placeholders with the same name - they will be replaced in order
* they appear in the message.
*/
class AccessibleLinksFormatter final
{
public:
   //! Handler to be called, when the Link is activated
   using LinkClickedHandler = std::function<void()>;

   /*! @brief Create AccessibleLinksFormatter using a TranslatableString.
    *  
    * TranslatableString may have the formatting options attached.
    * TranslatableString copy will be stored, so formatting options that are appended
    * after AccessibleLinksFormatter is created won't have any effect on the 
    * AccessibleLinksFormatter instance.
    */
   explicit AccessibleLinksFormatter(TranslatableString message);

   //! Replace placeholder with a link, that will open URL in default browser.
   AccessibleLinksFormatter& FormatLink(
      wxString placeholder, TranslatableString value, std::string targetURL);
   
   //! Replace placeholder with a link, that will call a callback provided.
   AccessibleLinksFormatter& FormatLink(
      wxString placeholder, TranslatableString value,
      LinkClickedHandler handler);

   //! Generate the UI.
   void Populate(ShuttleGui& S) const;
private:
   struct FormatArgument final
   {
      wxString Placeholder;
      TranslatableString Value;

      LinkClickedHandler Handler;
      std::string TargetURL;
   };

   struct ProcessedArgument final
   {
      const FormatArgument* Argument { nullptr };
      size_t PlaceholderPosition { wxString::npos };
   };

   /* Find the positions of the placeholders and sort 
    * arguments according to the positions. 
    */
   std::vector<ProcessedArgument>
   ProcessArguments(wxString translatedMessage) const;

   TranslatableString mMessage;
   std::vector<FormatArgument> mFormatArguments;
};
