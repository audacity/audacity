/**********************************************************************

  Sneedacity: A Digital Audio Editor

  HtmlWindow.cpp

  Leland Lucius

*******************************************************************//**

\file HtmlWindow.cpp

  Implements HtmlWindow

*//*******************************************************************//**

\class HtmlWindow
\brief The widget to the left of a ToolBar that allows it to be dragged
around to NEW positions.

*//**********************************************************************/

#ifndef __SNEEDACITY_WIDGETS_HtmlWindow__
#define __SNEEDACITY_WIDGETS_HtmlWindow__



#include <wx/setup.h> // for wxUSE_* macros
#include <wx/defs.h>
#include <wx/html/htmlwin.h> // to inherit

////////////////////////////////////////////////////////////
/// HtmlWindow Class
////////////////////////////////////////////////////////////

class SNEEDACITY_DLL_API HtmlWindow /* not final */ : public wxHtmlWindow
{
public:
   HtmlWindow(wxWindow *parent,
              wxWindowID id = wxID_ANY,
              const wxPoint& pos = wxDefaultPosition,
              const wxSize& size = wxDefaultSize,
              long style = wxHW_DEFAULT_STYLE,
              const wxString& name = wxT("htmlWindow"));
   virtual ~HtmlWindow();
};

#endif
