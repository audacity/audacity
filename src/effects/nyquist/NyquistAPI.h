/**********************************************************************

  Audacity: A Digital Audio Editor

  NyquistAPI.h

  Roger B. Dannenberg
  Apr 2021

  Interface for Nyquist access to tracks and more

**********************************************************************/

// (AUD-GET-TRACK-INFO id) -- get a property list from track by name or index
//     track index starts at 0, incremented by 1 for each multi-channel track
//     E.g. if there are two stereo tracks, they are tracks 0 and 1.
//     Properties are:
//         NAME - the name (STRING)
//         TYPE - WAVE, NOTE, or LABEL (SYMBOL)
//         For type WAVE:
//             CHANNELS - 1 or 2 (FIXNUM)
//             CLIPS - ((start end) (start end) ...) for mono (LIST)
//                     or #( ((s e) (s e) ...) ((s e) (s e) ...) ) for 
//                        stereo (ARRAY)
//         For type NOTE and LABEL:
//             no other properties, although if notes and labels were organized
//             into clips, it would be appropriate to add a CLIPS property. 
//             Notes and labels are accessed using AUD-GET-LABELS and 
//             AUD-GET-NOTES
//
// (AUD-GET-AUDIO id start dur) -- return sound or array of sounds.
//     id is name or number
//     start is project time for start of sound (FLONUM)
//     dur is duration in seconds (FLONUM)
//     zeros are inserted as necessary to achieve the requested duration
// 
// (AUD-PUT-AUDIO id expr start maxdur) -- write sound to a track
//     id is name or number
//     expr is a lisp expression to be evaluated to get a sound
//          the expr must evaluate to a mono or stereo sound matching the track
//     start is an offset time applied to the result of expr
//     maxdur is the maximum duration (relative to start) for the sound
//
// (AUD-GET-LABELS id start dur) -- return labels.
//     id is name or number
//     start is project time for start of label region (FLONUM)
//     dur is duration in seconds (FLONUM)
//     returns list of labels: ((T0 T1 "label") (T0 T1 "label") ...)
//         such that label T0 >= start and T0 < start + dur. 
//         Note that labels can have durations > 0 such that T1 > start + dur.
//
// (AUD-PUT-LABELS id labels merge) -- add/replace labels to track
//     id is name or number
//     labels is a label list in the format returned by AUD-GET-LABELS
//     if merge is NULL, replace all labels in the track. Otherwise, merge.
//
// (AUD-GET-NOTES id start dur inbeats) -- return notes and updates from track
//     If inbeats is NULL, all time units are in beats. Otherwise all units are 
//         seconds.
//     id is name or number
//     start is the project time for the start of a region (beats or seconds)
//     dur is the duration for a region (beats or seconds)
//     returns a list of events with time >= start and time < start + dur
//         each event in the returned list has one of two formats:
//         (NOTE key time channel pitch loudness duration 
//          [parameter parameter ...])
//              key is an event identifier, normally MIDI key number (FIXNUM)
//              time is the start time (beats or seconds) (FLONUM)
//              channel is the (usually MIDI) channel number (FIXNUM)
//              pitch is the exact pitch in (fractional) half-steps (FLONUM)
//              loudness is in MIDI velocity units (FLONUM)
//              duration is event duration in beats or seconds (FLONUM)
//              each parameter (optional) is a keyword followed by a value. 
//                  The keyword prepends a colon (":") to the Allegro attribute
//                  name and retains the type code suffix:
//                  s[tring], r[eal], i[nteger], or l[ogical].
//              Example: (NOTE 60 4.0 3 100.0 1.0 :colors "blue")
//         (UPDATE key time channel parameter)
//              key is an event identifier, normally MIDI key number (FIXNUM)
//              time is the start time (beats or seconds) (FLONUM)
//              channel is the (usually MIDI) channel number (FIXNUM)
//              parameter is an attribute keyword followed by value as in NOTE.
// 
// (AUD-PUT-NOTES id notes merge)
//     id is name or number
//     notes is an event list in the format returned by AUD-GET-NOTES
//     if merge is NULL, replace all events in the track. Otherwise, merge.
// 
// (AUD-GET-TIMES id start dur)
//     id is name or number
//     start is project time for start of region (FLONUM)
//     dur is duration in seconds (FLONUM)
//     returns list of breakpoints: ((x y) (x y) ...)
//         such that label x >= start and x < start + dur. 
//
// (AUD-PUT-TIMES id breakpoints) -- add/replace labels to track
//     id is name or number
//     breakpoints is a list in the format returned by AUD-GET-TIMES
//     The entire track content is always replaced by the new breakpoints.
//


#ifndef __AUDACITY_NYQUISTAPI__
#define __AUDACITY_NYQUISTAPI__

void NyquistAPICleanup();

LVAL getTrackInfo(LVAL nameOrNumber);
/* LISP: (AUD-GET-TRACK-INFO ANY) */

LVAL getAudio(LVAL nameOrNumber, double start, double dur);
/* LISP: (AUD-GET-AUDIO ANY ANYNUM ANYNUM) */

LVAL putAudio(LVAL nameOrNumber, LVAL expr, double start, double maxdur);
/* LISP: (AUD-PUT-AUDIO ANY ANY ANYNUM ANYNUM) */

LVAL getLabels(LVAL nameOrNumber, double start, double dur);
/* LISP: (AUD-GET-LABELS ANY ANYNUM ANYNUM) */

LVAL putLabels(LVAL nameOrNumber, LVAL labels, LVAL merge_flag);
/* LISP: (AUD-PUT-LABELS ANY ANY ANY) */

LVAL getNotes(LVAL nameOrNumber, double start, double dur);
/* LISP: (AUD-GET-NOTES ANY ANYNUM ANYNUM) */

LVAL putNotes(LVAL nameOrNumber, LVAL notes, LVAL merge_flag);
/* LISP: (AUD-PUT-NOTES ANY ANY ANY) */

LVAL getTimes(LVAL nameOrNumber, double start, double dur);
/* LISP: (AUD-GET-TIMES ANY ANYNUM ANYNUM) */

LVAL putTimes(LVAL nameOrNumber, LVAL breakpoints);
/* LISP: (AUD-PUT-TIMES ANY ANY) */

#endif
