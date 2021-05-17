/**********************************************************************

  Audacity: A Digital Audio Editor

  Clipboard.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_CLIPBOARD__
#define __AUDACITY_CLIPBOARD__



#include <memory>
#include <wx/event.h> // to inherit wxEvtHandler

class AudacityProject;
class TrackList;

// An event emitted by the clipboard whenever its contents change.
wxDECLARE_EXPORTED_EVENT( AUDACITY_DLL_API,
                          EVT_CLIPBOARD_CHANGE, wxCommandEvent );

class AUDACITY_DLL_API Clipboard final
   : public wxEvtHandler
{
public:
   static Clipboard &Get();

   const TrackList &GetTracks() const;

   double T0() const { return mT0; }
   double T1() const { return mT1; }
   double Duration() const { return mT1 - mT0; }

   const std::weak_ptr<AudacityProject> &Project() const { return mProject; }

   void Clear();
   
   void Assign(
     TrackList && newContents, double t0, double t1,
     const std::weak_ptr<AudacityProject> &pProject );

   Clipboard();
   ~Clipboard();

   void Swap( Clipboard &other );

private:

   std::shared_ptr<TrackList> mTracks;
   std::weak_ptr<AudacityProject> mProject{};
   double mT0{ 0 };
   double mT1{ 0 };
};

#endif
