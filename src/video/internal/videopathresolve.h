/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_VIDEO_VIDEOPATHRESOLVE_H
#define AU_VIDEO_VIDEOPATHRESOLVE_H

#include <functional>
#include <string>

namespace au::video {
//! Finding an attached video again after a project is reopened.
//!
//! This is the project format's first dependency on a file outside the
//! .aup3, so how the reference is stored matters. An absolute path alone
//! breaks as soon as the project is moved, sent to someone else, or opened on
//! a second machine through a sync folder. A relative path alone breaks as
//! soon as the media lives somewhere central rather than beside the project.
//!
//! Both are stored, and the relative one is tried first: a project and its
//! media moved together is the common case, and it is the case an absolute
//! path gets wrong.

//! Path of `absolute` relative to `projectDir`, or empty when there is no
//! sensible relative form - a different drive, or nothing above the project
//! directory worth walking up through.
std::string makeRelativeVideoPath(const std::string& projectDir, const std::string& absolute);

//! Picks whichever stored path still points at a file, relative first.
//! Returns empty when neither does.
//!
//! `exists` is injected so the whole thing is testable without touching a
//! disk. `projectDir` empty means the project has never been saved, in which
//! case only the absolute path can mean anything.
std::string resolveVideoPath(const std::string& absolute, const std::string& relative, const std::string& projectDir,
                             const std::function<bool(const std::string&)>& exists);
}

#endif // AU_VIDEO_VIDEOPATHRESOLVE_H
