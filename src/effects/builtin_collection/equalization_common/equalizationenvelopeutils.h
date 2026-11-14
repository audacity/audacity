/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

struct EqualizationFilter;

namespace au::effects::eq_common {
//! Convert mLogEnvelope points to mLinEnvelope (called when toggling
//! frequency scale from log to linear). No-op if mLogEnvelope is empty.
void envLogToLin(EqualizationFilter& parameters);

//! Convert mLinEnvelope points to mLogEnvelope (called when toggling
//! frequency scale from linear to log). No-op if mLinEnvelope is empty.
//! Returns true if a point had to be clamped to keep it inside [0, 1] of
//! the log domain (the caller may want to capture the resulting curve).
bool envLinToLog(EqualizationFilter& parameters);
}
