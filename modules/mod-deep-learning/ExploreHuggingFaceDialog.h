/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   ExploreHuggingFaceDialog.h
   Hugo Flores Garcia

******************************************************************/
/**

\file ExploreHuggingFaceDialog.h
\brief ExploreHuggingFaceDialog A dialog to direct users to a browser window that points users of 
                                 the Deep Learning suite to HuggingFace models available for download.

*/
/*******************************************************************/

#include <widgets/wxPanelWrapper.h>

class ExploreHuggingFaceDialog : public wxDialogWrapper
{
public:
   ExploreHuggingFaceDialog(wxWindow *parent);
};
