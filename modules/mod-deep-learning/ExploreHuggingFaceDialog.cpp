/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ExploreHuggingFaceDialog.cpp
   Hugo Flores Garcia

******************************************************************/

#include "ExploreHuggingFaceDialog.h"

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

ExploreHuggingFaceDialog::ExploreHuggingFaceDialog(wxWindow *parent)
   // i18n-hint: The "Models" here refer to neural network (AI) models for processing audio, for a variety of uses. 
   : wxDialogWrapper(parent, wxID_ANY, XO("Explore Models"))
{
   ShuttleGui S(this, eIsCreating);
   S.StartStatic(Verbatim(""), true);
   {
      S.AddFixedText(
         XO(
            "Deep learning models for Audacity are contributed by the open-source \n"
            "community and are hosted in HuggingFace. You can explore models for Audacity\n"
            "by clicking the following link: "
         )
      );

      S.AddWindow(
         safenew wxHyperlinkCtrl(
            S.GetParent(), wxID_ANY, 
            "https://huggingface.co/models?filter=audacity",
            "https://huggingface.co/models?filter=audacity"
         ) 
      );

      S.AddFixedText(
         XO(
            "To add a new model to your local collection, use the \n"
            "\"Add From HuggingFace\" button."
         )
      );
   }

   Fit();
   Layout();
   Center();
   SetMinSize(GetSize());
   Refresh();
}
