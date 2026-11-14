/**********************************************************************

  Audacity: A Digital Audio Editor

  Clipboard.h

  Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_CLIPBOARD__
#define __AUDACITY_CLIPBOARD__

#include <memory>

#include "Observer.h"

class AudacityProject;
class TrackList;

//! Message is sent during idle time by the global clipboard
struct ClipboardChangeMessage {};

class AUDACITY_DLL_API Clipboard final : public Observer::Publisher<ClipboardChangeMessage>
{
public:
    static Clipboard& Get();

    const TrackList& GetTracks() const;

    double T0() const { return mT0; }
    double T1() const { return mT1; }
    double Duration() const { return mT1 - mT0; }

    const std::weak_ptr<AudacityProject>& Project() const { return mProject; }

    void Clear();

    /*!
     @pre `!newContents.GetOwner()`
     */
    void Assign(
        TrackList&& newContents, double t0, double t1, const std::weak_ptr<AudacityProject>& pProject);

    ~Clipboard();

    void Swap(Clipboard& other);

    struct AUDACITY_DLL_API Scope;

private:
    Clipboard();
    Clipboard(const Clipboard&) = delete;
    Clipboard& operator=(const Clipboard&) = delete;

    /*!
     @invariant `!mTracks->GetOwner()`
     */
    std::shared_ptr<TrackList> mTracks;
    std::weak_ptr<AudacityProject> mProject{};
    double mT0{ 0 };
    double mT1{ 0 };
};

//! Empty the clipboard at start of scope; restore its contents after
struct Clipboard::Scope {
    Scope();
    ~Scope();
private:
    Clipboard mTempClipboard;
};

#endif
