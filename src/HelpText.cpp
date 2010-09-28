/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpText.cpp

  James Crook

********************************************************************//**

\file HelpText.cpp
\brief Given a key, returns some html.
*//********************************************************************/

#include <wx/string.h>
#include <wx/intl.h>

#include "Audacity.h"
#include "HelpText.h"
#include "FileNames.h"



wxString WrapText( const wxString & Text )
{
   return wxString(wxT(""))+
      wxT("<html><head></head>") +
      wxT("<body bgcolor=\"#ffffff\">") +
      wxT("<p>") + Text +
      wxT("</body></html>");
}

wxString Link( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='innerlink:") +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

wxString WikiLink( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='http://www.audacityteam.org/wiki/index.php?title=") +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

wxString FileLink( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='") +
      wxT("file:") +
      FileNames::HtmlHelpDir() +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

wxString HttpLink( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='") +
      wxT("http:") +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

wxString LinkExpand( const wxString & Text )
{
   wxString Temp = Text;
   int i,j,k;
   while( (i=Temp.First( wxT("[[") ))!= wxNOT_FOUND )
   {
      wxString Key = Temp.Mid(i+2);
      j = Key.First( wxT("|") );
      if( j==wxNOT_FOUND )
         return Temp;
      wxString LinkText = Key.Mid( j+1);
      k = LinkText.First( wxT("]]") );
      if( k==wxNOT_FOUND )
         return Temp;
      Key = Key.Mid( 0, j );
      LinkText = LinkText.Mid( 0, k );

      wxString Replacement;
      if( Key.StartsWith( wxT("wiki:") ))
      {
         Replacement = WikiLink( Key.Mid( 5 ), LinkText );
      }
      else if( Key.StartsWith( wxT("file:") ))
      {
         Replacement = FileLink( Key.Mid( 5 ), LinkText );
      }
      else if( Key.StartsWith( wxT("http:") ))
      {
         Replacement = HttpLink( Key.Mid( 5 ), LinkText );
      }
      else
      {
         Replacement = Link( Key, LinkText );
      }

      Temp = Temp.Mid( 0, i ) + Replacement + Temp.Mid( i + j + k + 5 );// 5 for the [[|]]
   }
   return Temp;
}

wxString TitleText( const wxString & Key )
{
   if(Key==wxT("welcome"))
   {
      return _("Welcome!");
   }

   if(Key ==wxT("play") )
   {
      return _("Playing Audio");
   }
   if((Key ==wxT("record") ) || (Key ==wxT("norecord") ))
   {
      return _("Recording Audio");
   }
   if(Key ==wxT("inputdevice") )
   {
      return _("Recording - Choosing the Input Device");
   }
   if(Key ==wxT("inputsource") )
   {
      return _("Recording - Choosing the Input Source");
   }
   if(Key ==wxT("inputlevel") )
   {
      return _("Recording - Setting the Input Level");
   }
   if((Key ==wxT("edit") ) || (Key==wxT("grey")))
   {
      return _("Editing and greyed out Menus");
   }
   if(Key ==wxT("export") )
   {
      return _("Exporting an Audio File");
   }
   if(Key ==wxT("save") )
   {
      return _("Saving an Audacity Project");
   }
   if(Key ==wxT("wma-proprietary") )
   {
      return _("Unsupported Formats");
   }
   if(Key ==wxT("burncd") )
   {
      return _("Burn to CD" );
   }
   if(Key ==  wxT("remotehelp") )
   {
      return _("No Local Help");
   }
   return Key;
}

wxString HelpTextBuiltIn( const wxString & Key )
{
   if(Key==wxT("welcome"))
   {
      return WrapText(
         wxString(wxT("")) +
         _("<center><h3>How to Get Help</h3></center>") +
         _("Welcome to Audacity ") + AUDACITY_VERSION_STRING + wxT("!<p>") +
         _("These are our support methods:") + wxT("</p>") + wxT("<ul><li>") + 
         _(" [[file:quick_help.html|Quick Help]] (should be installed locally, <a href=\"http://manual.audacityteam.org/index.php?title=Quick_Help\">Internet version if it isn't</a>)") + wxT("</li><li>") +
         _(" [[file:index.html|Manual]] (should be installed locally, <a href=\"http://manual.audacityteam.org/index.php\">Internet version if it isn't</a>)") + wxT("</li><li>") +
         _(" [[http://wiki.audacityteam.org/index.php|Wiki]] (the latest tips, tricks and tutorials, on the Internet)") + wxT("</li><li>") +
         _(" <a href=\"http://forum.audacityteam.org/\">Forum</a> (ask your question directly, on the Internet)") + wxT("</li></ul></p><p>") +
         _(" For even quicker answers, all the online resources above are <b>searchable</b>.")  + wxT("</p>") 
      );
   }

   // Remote help allows us to link to a local copy of the help if it exists,
   // or provide a message that takes you to the Internet if it does not.
   // It's used by the menu item Help > Index
   if(Key ==  wxT("remotehelp") )
   {
// *URL* will be replaced by whatever URL we are looking for.
      return WrapText(_("<p>You do not appear to have 'help' installed on your computer.<br> \
Please <a href=\"*URL*\">view or download it online</a>.")
         );
   }
   return wxT("");
}

wxString HelpText( const wxString & Key )
{

   // Possible future enhancement...
   // We could look for the text as a local file and use
   // that if we find it...
   // if( wxFileExists( Path+Key ) )
   // ...

   wxString Text;
   Text = HelpTextBuiltIn( Key );

   if( !Text.IsEmpty())
      return LinkExpand( Text );

   // Perhaps useful for debugging - we'll return key that we didn't find.
   return WrapText( Key );
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: a8955864-40e2-47aa-923b-cace3994493a

