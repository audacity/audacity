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



static wxString WrapText( const wxString & Text )
{
   return wxString(wxT(""))+
      wxT("<html><head></head>") +
      wxT("<body bgcolor=\"#ffffff\">") +
      wxT("<p>") + Text +
      wxT("</body></html>");
}

static wxString Link( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='innerlink:") +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

static wxString WikiLink( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='http://www.audacityteam.org/wiki/index.php?title=") +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

static wxString FileLink( const wxString &Key, const wxString& Text )
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

static wxString HttpLink( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='") +
      wxT("http:") +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

static wxString LinkExpand( const wxString & Text )
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
      /* i18n-hint: Title for a topic.*/
      return _("Playing Audio");
   }
   if((Key ==wxT("record") ) || (Key ==wxT("norecord") ))
   {
      /* i18n-hint: Title for a topic.*/
      return _("Recording Audio");
   }
   if(Key ==wxT("inputdevice") )
   {
      /* i18n-hint: Title for a topic.*/
      return _("Recording - Choosing the Recording Device");
   }
   if(Key ==wxT("inputsource") )
   {
      /* i18n-hint: Title for a topic.*/
      return _("Recording - Choosing the Recording Source");
   }
   if(Key ==wxT("inputlevel") )
   {
      /* i18n-hint: Title for a topic.*/
      return _("Recording - Setting the Recording Level");
   }
   if((Key ==wxT("edit") ) || (Key==wxT("grey")))
   {
      /* i18n-hint: Title for a topic.*/
      return _("Editing and greyed out Menus");
   }
   if(Key ==wxT("export") )
   {
      /* i18n-hint: Title for a topic.*/
      return _("Exporting an Audio File");
   }
   if(Key ==wxT("save") )
   {
      /* i18n-hint: Title for a topic.*/
      return _("Saving an Audacity Project");
   }
   if(Key ==wxT("wma-proprietary") )
   {
      /* i18n-hint: Title for a topic.*/
      return _("Support for Other Formats");
   }
   if(Key ==wxT("burncd") )
   {
      /* i18n-hint: Title for a topic.*/
      return _("Burn to CD" );
   }
   if(Key ==  wxT("remotehelp") )
   {
      return _("No Local Help");
   }
   return Key;
}

static wxString HelpTextBuiltIn( const wxString & Key )
{
   if(Key==wxT("welcome"))
   {
      /// TO-DO: Make the links to help here use the widgets/HelpSystem mechanism
	  /// so that they are consistent
      /* i18n-hint: Preserve [[file:quick_help.html as it's the name of a file.*/
      return WrapText(
         wxString(wxT("")) +
         _("<center><h3>How to Get Help</h3></center>") +
         _("Welcome to Audacity ") + AUDACITY_VERSION_STRING + wxT("!<p>") +
         _("These are our support methods:") + wxT("</p>") + wxT("<ul><li>") +
         _(" [[file:quick_help.html|Quick Help]] (should be installed locally, <a href=\"http://manual.audacityteam.org/o/quick_help.html\">Internet version if it isn't</a>)") + wxT("</li><li>") +
         _(" [[file:index.html|Manual]] (should be installed locally, <a href=\"http://manual.audacityteam.org/o/\">Internet version if it isn't</a>)") + wxT("</li><li>") +
         _(" [[http://wiki.audacityteam.org/index.php|Wiki]] (the latest tips, tricks and tutorials, on the Internet)") + wxT("</li><li>") +
         _(" <a href=\"http://forum.audacityteam.org/\">Forum</a> (ask your question directly, on the Internet)") + wxT("</li></ul></p><p>") +
         _(" For even quicker answers, all the online resources above are <b>searchable</b>.")  + wxT("</p>")
      );
   }
   if(Key==wxT("wma-proprietary"))
   {
      return WrapText(
         wxString(wxT("<p>"))+
         _("Audacity can import unprotected files in many other formats (such as M4A and WMA, \
compressed WAV files from portable recorders and audio from video files) if you download and install \
the optional <a href=\"http://manual.audacityteam.org/o/man/faq_opening_and_saving_files.html#foreign\"> \
FFmpeg library</a> to your computer.") + wxT("</p><p>") +
         _("You can also read our help on importing \
<a href=\"http://manual.audacityteam.org/o/man/faq_opening_and_saving_files.html#midi\">MIDI files</a> \
and tracks from <a href=\"http://manual.audacityteam.org/o/man/faq_opening_and_saving_files.html#fromcd\"> \
audio CDs</a>.") + wxT("</p>")
      );
   }

   // Remote help allows us to link to a local copy of the help if it exists,
   // or provide a message that takes you to the Internet if it does not.
   // It's used by the menu item Help > Index
   if(Key ==  wxT("remotehelp") )
   {
// *URL* will be replaced by whatever URL we are looking for.
      return WrapText(_("You do not appear to have the 'help' folder installed.<br> \
Please <a href=\"*URL*\">view the content online</a> or \
<a href=\"http://manual.audacityteam.org/o/man/unzipping_the_manual.html\"> \
download the full Manual</a>.")
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
