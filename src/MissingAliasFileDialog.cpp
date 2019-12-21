/*********************************************************************

\class MissingAliasFileDialog
\brief Special case of ErrorDialog for reporting missing alias files.

*//*******************************************************************/

#include "MissingAliasFileDialog.h"

#include <mutex>
#include <wx/frame.h>

#include "BlockFile.h"
#include "DirManager.h"
#include "Project.h"
#include "widgets/ErrorDialog.h"

namespace {
   using wxDialogRef = wxWeakRef< wxDialog >;
   std::vector< wxDialogRef > sDialogs;
}

// special case for alias missing dialog because we keep track of if it exists.
class MissingAliasFileDialog final : public ErrorDialog
{
   public:
   MissingAliasFileDialog(wxWindow *parent,
      const TranslatableString & dlogTitle,
      const TranslatableString & message,
      const wxString & helpURL,
      const bool Close = true, const bool modal = true);
   virtual ~MissingAliasFileDialog();
};


MissingAliasFileDialog::MissingAliasFileDialog(wxWindow *parent,
      const TranslatableString & dlogTitle,
      const TranslatableString & message,
      const wxString & helpURL,
      const bool Close, const bool modal)
: ErrorDialog( parent,
               dlogTitle, message, helpURL, Close, modal )
{
   sDialogs.push_back( this );
}

MissingAliasFileDialog::~MissingAliasFileDialog()
{
   auto begin = sDialogs.begin(), end = sDialogs.end(),
      newEnd = std::remove_if( begin, end,
         [&]( const wxDialogRef &ref ){
            return ref == this; } );
   sDialogs.erase( newEnd, end );
}

namespace MissingAliasFilesDialog {
   
   namespace{
      bool                 m_missingAliasFilesWarningShouldShow{ true };
      std::weak_ptr< AudacityProject > m_LastMissingBlockFileProject;
      wxString             m_LastMissingBlockFilePath;
      std::mutex m_LastMissingBlockFileLock;
   }
   
   using Lock = std::unique_lock< std::mutex >;
   
   void Show(AudacityProject *project,
             const TranslatableString &dlogTitle,
             const TranslatableString &message,
             const wxString &helpPage,
             const bool Close)
   {
      auto parent = FindProjectFrame( project );
      wxASSERT(parent); // to justify safenew
      ErrorDialog *dlog = safenew MissingAliasFileDialog(parent, dlogTitle, message, helpPage, Close, false);
      // Don't center because in many cases (effect, export, etc) there will be a progress bar in the center that blocks this.
      // instead put it just above or on the top of the project.
      wxPoint point;
      point.x = 0;
      
      point.y = parent ? parent->GetPosition().y - 200 : 100;
      
      if (point.y < 100)
         point.y = 100;
      dlog->SetPosition(point);
      dlog->CentreOnParent(wxHORIZONTAL);
      
      // This needs to be modeless because user may need to
      // stop playback AND read dialog's instructions.
      dlog->Show();
      // ANSWER-ME: Vigilant Sentry flags this method as not deleting dlog, so a mem leak.
      // PRL: answer is that the parent window guarantees destruction of the dialog
      // but in practice Destroy() in OnOK does that
   }
   
   wxDialog *Find( const AudacityProject &project )
   {
      auto &window = GetProjectFrame( project );
      auto begin = sDialogs.begin(), end = sDialogs.end(),
         iter = std::find_if( begin, end,
            [&]( const wxDialogRef &ref ){
               return ref && ref->GetParent() == &window; } );
      if (iter != end)
         return *iter;
      return nullptr;
   }

   void Mark(const AliasBlockFile *b)
   {
      Lock lock{ m_LastMissingBlockFileLock };
      if (b) {
         for ( auto pProject : AllProjects{} ) {
            // search each project for the blockfile
            if (DirManager::Get( *pProject ).ContainsBlockFile(b)) {
               m_LastMissingBlockFileProject = pProject;
               break;
            }
         }
      }
      else
         m_LastMissingBlockFileProject = {};
      
      if (b)
         m_LastMissingBlockFilePath = b->GetAliasedFileName().GetFullPath();
      else
         m_LastMissingBlockFilePath = wxString{};
   }
   
   std::pair< wxString, std::shared_ptr<AudacityProject> > Marked()
   {
      Lock lock{ m_LastMissingBlockFileLock };
      return { m_LastMissingBlockFilePath, m_LastMissingBlockFileProject.lock() };
   }
   
   bool ShouldShow()
   {
      Lock lock{ m_LastMissingBlockFileLock };
      auto ptr = m_LastMissingBlockFileProject.lock();
      return ptr && m_missingAliasFilesWarningShouldShow;
   }
   
   void SetShouldShow(bool b)
   {
      // Note that this is can be called by both the main thread and other threads.
      // I don't believe we need a mutex because we are checking zero vs non-zero,
      // and the setting from other threads will always be non-zero (true), and the
      // setting from the main thread is always false.
      m_missingAliasFilesWarningShouldShow = b;
      // reset the warnings as they were probably marked by a previous run
      if (m_missingAliasFilesWarningShouldShow) {
         Mark( nullptr );
      }
   }
   
}

// Arrange callback from low levels of block file I/O to detect missing files
static struct InstallHook{ InstallHook() {
   auto hook = [](const AliasBlockFile *pAliasFile){
      if (!MissingAliasFilesDialog::ShouldShow())
         MissingAliasFilesDialog::Mark(pAliasFile);
   };
   BlockFile::SetMissingAliasFileFound( hook );
} } installHook;
