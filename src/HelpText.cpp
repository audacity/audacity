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
#include "AboutDialog.h"
#include "AllThemeResources.h"


wxString HtmlColourOfIndex( int i ){
   wxColour c =  theTheme.Colour(i);
   return wxString::Format("\"#%02X%02X%02X\"",
      c.Red(), c.Green(), c.Blue() );
}

static wxString WrapText( const wxString & Text )
{
   return wxString(wxT(""))+
      wxT("<html><head></head>") +
      wxT("<body bgcolor=") + HtmlColourOfIndex(clrTrackInfo) + wxT(">") +
      wxT("<font color=") + HtmlColourOfIndex(clrTrackPanelText) + wxT(">") +
      wxT("<p>") + Text +
      wxT("</font>")+
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

static wxString TypedLink( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='") +
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

      LinkText=wxString("<font color=") + HtmlColourOfIndex(clrSample) + wxT(">") +LinkText+"</font>";
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
         Replacement = TypedLink( Key, LinkText );
      }
      else if( Key.StartsWith( wxT("mailto:") ))
      {
         Replacement = TypedLink( Key, LinkText );
      }
      else if( Key.StartsWith( wxT("*URL*") ))
      {
         Replacement = TypedLink( Key, LinkText );
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
      wxString result = 
         wxString(wxT("")) + 
#if IS_ALPHA
         wxT("<hr><center><h3>") + _("Get the Official Released Version of Audacity") + wxT("</h3></center>") +
         VerCheckHtml() +
         _("<br><br>The version of Audacity you are using is an <b>Alpha test version</b>.") + " " +
         _("We strongly recommend that you use our latest stable released version, which has full documentation and support.<br><br>")+
         _("You can help us get Audacity ready for release by joining our [[http://www.audacityteam.org/community/|community]].<hr><br><br>")+
#endif   

// DA: Support methods text.
#ifdef EXPERIMENTAL_DA
         wxT("<center><h3>DarkAudacity ") + AUDACITY_VERSION_STRING + wxT("</h3></center>") +
         _("<br><br>DarkAudacity is based on Audacity:") + wxT("<ul><li>") +
         _(" [[http://www.darkaudacity.com|www.darkaudacity.com]] - for differences between them.") + wxT("</li><li>") +
         _(" email to [[mailto:james@audacityteam.org|james@audacityteam.org]] - for help using DarkAudacity.") + wxT("</li><li>") +
         _(" [[http://www.darkaudacity.com/video.html|Tutorials]] - for getting started with DarkAudacity.") + wxT("</li></ul>") +

         wxT("<br><br>Audacity has these support methods:") + wxT("<ul><li>") +
         wxT(" [[http://manual.audacityteam.org/|Manual]] - for comprehensive Audacity documentation") + wxT("</li><li>") +
         wxT(" [[http://forum.audacityteam.org/|Forum]] - for large knowledge base on using Audacity.") + wxT("</li></ul>");
#else
         wxT("<center><h3>Audacity ") + AUDACITY_VERSION_STRING + wxT("</h3><h3>") +
         _("How to get help") + wxT("</h3></center>") + 
         _("These are our support methods:") + wxT("<p><ul><li>") +
         _(" [[file:quick_help.html|Quick Help]] - if not installed locally, [[http://manual.audacityteam.org/quick_help.html|view online]]") + wxT("</li><li>") +
         _(" [[file:index.html|Manual]] - if not installed locally, [[http://manual.audacityteam.org/|view online]]") + wxT("</li><li>") +
         _(" [[http://forum.audacityteam.org/|Forum]] - ask your question directly, online.") + wxT("</li></ul></p><p>") + wxT("<b>") + 
         _("More:</b> Visit our [[http://wiki.audacityteam.org/index.php|Wiki]] for tips, tricks, extra tutorials and effects plug-ins.") + wxT("</p>");
#endif

#if IS_ALPHA
      result.Replace( "//manual.audacityteam.org/quick_help.html","//alphamanual.audacityteam.org/man/Quick_Help" );
      result.Replace( "//manual.audacityteam.org/","//alphamanual.audacityteam.org/man/" );
#endif

      return WrapText( result );
   }
   if(Key==wxT("wma-proprietary"))
   {
      return WrapText(
         wxString(wxT("<p>"))+
         _("Audacity can import unprotected files in many other formats (such as M4A and WMA, \
compressed WAV files from portable recorders and audio from video files) if you download and install \
the optional [[http://manual.audacityteam.org/man/faq_opening_and_saving_files.html#foreign| \
FFmpeg library]] to your computer.") + wxT("</p><p>") +
         _("You can also read our help on importing \
[[http://manual.audacityteam.org/man/faq_opening_and_saving_files.html#midi|MIDI files]] \
and tracks from [[http://manual.audacityteam.org/man/faq_opening_and_saving_files.html#fromcd| \
audio CDs]].") + wxT("</p>")
      );
   }

   // Remote help allows us to link to a local copy of the help if it exists,
   // or provide a message that takes you to the Internet if it does not.
   // It's used by the menu item Help > Index
   if(Key ==  wxT("remotehelp") )
   {
// *URL* will be replaced by whatever URL we are looking for.
// DA: View the manual on line is expected.
#ifdef EXPERIMENTAL_DA
      return WrapText(_("The Manual does not appear to be installed. \
Please [[*URL*|view the Manual online]].<br><br>\
To always view the Manual online, change \"Location of Manual\" in \
Interface Preferences to \"From Internet\".")
#else
      return WrapText(_("The Manual does not appear to be installed. \
Please [[*URL*|view the Manual online]] or \
[[http://manual.audacityteam.org/man/unzipping_the_manual.html| \
download the Manual]].<br><br>\
To always view the Manual online, change \"Location of Manual\" in \
Interface Preferences to \"From Internet\".")
#endif
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


wxString FormatHtmlText( const wxString & Text ){

   wxString localeStr = wxLocale::GetSystemEncodingName();

   return 
      wxT("<html><head><META http-equiv=\"Content-Type\" content=\"text/html; charset=") +
      localeStr +
      wxT("\"></head>") +
      WrapText( LinkExpand( Text ))+
      wxT("</html>");
}
