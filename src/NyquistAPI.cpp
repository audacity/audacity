/**********************************************************************

  Audacity: A Digital Audio Editor

  NyquistAPI.cpp

  Roger B. Dannenberg
  Apr 2021

  Interface for Nyquist access to tracks and more

**********************************************************************/


#include "../lib-src/libnyquist/nyquist/xlisp/xlisp.h"
#include "NyquistAPI.h"
#include "WaveClip.h"
#include "LabelTrack.h"
#include "nyx.h"
#include "../lib-src/portsmf/allegro.h"

// To access project information, effect runner finds and shares project here:
const AudacityProject *audacityProject = NULL;
void setNyquistProject(const AudacityProject *p)
{
    audacityProject = p;
}

LVAL properties(const Track *track)
{
    LVAL props, last, allClips, clips, lastClips, cl;
    xlstkcheck(4);
    xlsave(props);
    xlsave(allClips);
    xlsave(clips);
    xlsave(lastClips);
    if (track) {
        wxString wname = track->GetName();
        const char *name = wname.mb_str();
        last = props = consa(xlenter("NAME"));
        last = rplacd(last, consa(name ? cvstring(name) : NULL));
        last = rplacd(last, consa(xlenter("TYPE")));
        const char* kindString = NULL;
        track->TypeSwitch([&](const WaveTrack* t) { kindString = "WAVE"; },
                          [&](const NoteTrack* t) { kindString = "NOTE"; },
                          [&](const LabelTrack* t) { kindString = "LABEL"; });
        last = rplacd(last, consa(kindString ? xlenter(kindString) : NULL));
        track->TypeSwitch([&](const WaveTrack* waveTrack) {
            auto channels = TrackList::Channels(waveTrack); // track->GetOwner();

            int nchans = channels.size();  // channels->size();
            last = rplacd(last, consa(xlenter("CHANNELS")));
            last = rplacd(last, consa(cvfixnum(nchans)));

            // compute the clips list as property "CLIPS":
            if (nchans > 1) {
                allClips = newvector(nchans);
            }

            int channel_index = 0;
            for (auto channel : channels) {
                clips = NULL;
                lastClips = NULL;

                auto ca = channel->SortedClipArray();
                for (size_t j = 0; j < ca.size(); j++) {
                    if (j < 1000) {
                        auto caj = ca[j]->GetStartTime();
                        cl = cons(cvflonum(ca[j]->GetStartTime()),
                            consa(cvflonum(ca[j]->GetEndTime())));
                        cl = consa(cl); // ((start end))
                        if (!clips) {
                            clips = cl;
                        }
                    }
                    else if (j == 1000) {
                        cl = consa(NULL); // (nil)
                    }
                    else {
                        break;
                    }
                    if (lastClips) {
                        lastClips = rplacd(lastClips, cl);
                    }
                    else {
                        lastClips = clips = cl;
                    }
                }
                if (allClips) {
                    setelement(allClips, channel_index++, clips);
                }
            }
            if (!allClips) {  // only one channel
                allClips = clips;
            }
            last = rplacd(last, consa(xlenter("CLIPS")));
            last = rplacd(last, consa(allClips));
            });
    }   
    xlpopn(4);
    return props;
}

const Track *getTrackByNumber(FIXTYPE n)
{
    const TrackList &trackList = TrackList::Get(*audacityProject);
    auto range = trackList.Leaders();

    if (n < 0 || n >= trackList.size()) {
        return NULL;
    }
    TrackList::const_iterator iter = range.begin();
    for (int i = 0; i < n; i++) iter++;  // advance to nth track
     return *iter;
}

const Track *getTrackByName(const char *name)
{
    const TrackList& trackList = TrackList::Get(*audacityProject);
    auto range = trackList.Leaders();

    for (auto ptrack : range) {
        wxString wname = ptrack->GetName();
        const char* trackName = wname.mb_str();
        if (strcmp(name, trackName) == 0) {
            return ptrack;
        }
    }
    return NULL;
}


const Track *getTrack(LVAL nameOrNumber)
{
    if (fixp(nameOrNumber)) {
        return getTrackByNumber(getfixnum(nameOrNumber));
    } else if (stringp(nameOrNumber)) {
        return getTrackByName((const char *)getstring(nameOrNumber));
    }
    return NULL;
}


LVAL getTrackInfo(LVAL nameOrNumber)
{
    const Track* track = getTrack(nameOrNumber);
    return properties(track);
}


// getAudio() creates a nyx_susp object to a source of samples
// This object only has a pointer to denote the state, but we
// need a buffer and other state, so we create a new class.
//
// A WaveTrackReader is allocated for each channel. When the effect
// completes, we need to delete the WaveTrackReaders, so keep them
// on a vector. If a sound terminates, we'll keep the WaveTrackReader
// anyway until the end of the effect.
//
// TODO: I think Nyquist effects can retain state and therefore sounds
// from one run to the next. Nyx should tie into the GC and keep
// backpointers to nyx_susp objects, changing their callbacks to 
// return zero if they are invoked after the end of an effect. There is
// no problem retaining a sound as long as it is no longer "lazy" and
// will not call back into Audacity looking for samples.

class WaveTrackReader;
static std::vector<WaveTrackReader*> waveTrackReaders;


class WaveTrackReader
{
public:
    sampleCount mCurBufferOffset;  // offset where reading starts
    // track samples are copied to this intermediate buffer
    SampleBuffer mCurBuffer;
    sampleCount  mCurBufferStart;  // sample count of first sample
    size_t       mCurBufferLen;  // length in samples of mCurBuffer
    
    const WaveTrack *mCurTrack;

    WaveTrackReader(const WaveTrack *track, double start) {
        mCurTrack = track;
        double srate = track->GetRate();
        mCurBufferOffset = (sampleCount)(start * srate + 0.5);
        mCurBufferLen = 0;
        // save all created objects and free them with NyquistAPICleanup()
        // after the effect has completed:
        waveTrackReaders.push_back(this);
    }

    // called from Nyquist sound object to implement lazy computation of
    // samples
    //   buffer - where to put samples
    //   channel - channel number (passed on, but ultimately ignored)
    //   start - sample count for sound (starts from zero)
    //   len - number of samples to be read and returned
    //         (caller assumes len is fulfilled, so zero fill if needed)
    //   totlen - total sound length in samples (passed on, but ultimately
    //            ignored); caller may pass zero for "unknown"
    //
    // TODO: remove channel and totlen parameters (affects Nyquist effect)
    static int StaticGetCallback(float* buffer, int channel,
        int64_t start, int64_t len, int64_t totlen, void *userdata) {
        WaveTrackReader *wtr = (WaveTrackReader *)userdata;
        return wtr->GetCallback(buffer, channel, start, len, totlen);
    }

    // note: near duplicate code from Nyquist.cpp (NyquistEffect)
    //
    // see StaticGetCallback for parameter descriptions
    int GetCallback(float *buffer, int WXUNUSED(ch),
                    int64_t start, int64_t len, int64_t WXUNUSED(totlen))
    {
        // Algorithm: either copy already-read samples out of mCurBuffer or
        //   start fresh: allocate buffer, fill it completely and copy from it
        if (mCurBuffer.ptr()) {
            // the first compare only happens if we do not read sequentially,
            // so it should never happen:
            if (mCurBufferOffset + start < mCurBufferStart || 
                mCurBufferOffset + start + len > 
                mCurBufferStart + mCurBufferLen) {
                mCurBuffer.Free();
            }
        }

        if (!mCurBuffer.ptr()) {
            mCurBufferStart = mCurBufferOffset + start;
            mCurBufferLen = mCurTrack->GetBestBlockSize(mCurBufferStart);

            if (mCurBufferLen < (size_t)len) {
                mCurBufferLen = ((WaveTrack *)mCurTrack)->GetIdealBlockSize();
            }
            
            // We don't have an end time (length) for the track
            // mCurBufferLen =
            //    limitSampleBufferSize(mCurBufferLen, mCurLen - mCurBufferStart);

            mCurBuffer.Allocate(mCurBufferLen, floatSample);

            // We're inside the Effects's try block, so none here?
            // here's the action: pull samples from track
            // try {
                mCurTrack->Get(
                    mCurBuffer.ptr(), floatSample,
                    mCurBufferStart, mCurBufferLen);
            /* }
            catch (...) {
                // Save the exception object for re-throw when out of the library
                mpException = std::current_exception();
                return -1;
            } */
        }

        // We have guaranteed above that this is nonnegative and bounded by
        // mCurBufferLen:
        auto offset = (mCurBufferOffset + start - mCurBufferStart).as_size_t();
        CopySamples(mCurBuffer.ptr() + offset * SAMPLE_SIZE(floatSample), floatSample,
                    (samplePtr)buffer, floatSample, len);

        return 0;
    }
};


int64_t getTrackLen(const WaveTrack *waveTrack)
{
    auto channels = TrackList::Channels(waveTrack); // track->GetOwner();
    double srate = waveTrack->GetRate();
    int64_t len = 0;
    for (auto channel : channels) {
        auto ca = channel->SortedClipArray();
        int n = ca.size();
        if (n > 0) {
            auto clip = ca[n - 1];  // last clip
            int64_t clipLen = (int64_t)(clip->GetEndTime() * srate + 0.5);
            if (clipLen > len) len = clipLen;
        }
    }
    return len;
}


// XLISP primitive implements AUD-GET-AUDIO. start is the track time
//   returns a sound with a start time of zero, i.e. the source is
//   shifted by -start to to start at time 0.
LVAL getAudio(LVAL nameOrNumber, double start, double dur)
{
    LVAL snd;
    xlsave1(snd);
    const Track* track = getTrack(nameOrNumber);
    if (track) {
        track->TypeSwitch([&](const WaveTrack* waveTrack) {
            auto channels = TrackList::Channels(waveTrack); // track->GetOwner();
            int nchans = channels.size();  // channels->size();
            double srate = waveTrack->GetRate();
            int64_t len = (int64_t)(dur * srate + 0.5);
            if (nchans > 1) {
                snd = newvector(nchans);
                int ch = 0;
                for (auto channel : channels) {
                    WaveTrackReader* wtr = new WaveTrackReader(channel, start);
                    setelement(snd, ch, nyx_make_input_audio(
                        WaveTrackReader::StaticGetCallback, wtr, 1,
                        len, waveTrack->GetRate()));
                    ch++;
                }
            }
            else {
                WaveTrackReader* wtr = new WaveTrackReader(waveTrack, start);
                snd = nyx_make_input_audio(
                    WaveTrackReader::StaticGetCallback, wtr, 1,
                    len, waveTrack->GetRate());
            }
        });
    }
    // if not a WaveTrack, fall through, return NULL
    xlpop();
    return snd;
}


class WaveTrackWriter
{
public:
    const char *error;
    WaveTrack* mCurTrack[2];
    WaveTrack* mOutputTrack[2];
    double mOutputTime;
    double srate;  // all channels must match

    // trkchans is the number of channels in the output track
    // chans in the number of channels in snd
    WaveTrackWriter(TrackIterRange<const WaveTrack>& channels, LVAL snd,
                    double rate, int trkchans, int chans,
                    double start, double maxdur) {
        error = NULL;
        mCurTrack[0] = mCurTrack[1] = nullptr;
        mOutputTrack[0] = mOutputTrack[1] = nullptr;
        mOutputTime = 0;
        srate = rate;
        std::shared_ptr<WaveTrack> outputTrack[2];

        if (maxdur < 0) {
            error = "Error: specified max duration < 0,";
            return;
        }

        // for now, require every output channel to have the same sample rate
        // the first channel already matches the Nyquist sound sample rate
        for (auto track : channels) {
            if (track->GetRate() != srate) {
                error = "Error: mismatched sample rates.";
                return;
            }
        }
        int i = 0;
        for (auto track : channels) {
            // this must be wrong. Also, I'm not sure how to write to a track.
            // I generally copied code from Nyquist.cpp (nyquist effects), but
            // I don't understand why it makes copies or how deep the copies 
            // go (are these virtual copies of whole tracks, or just empty
            // containers for new audio?)
            mCurTrack[i] = (WaveTrack *) (const WaveTrack *) track;
            outputTrack[i] = track->EmptyCopy();
            outputTrack[i]->SetRate(srate);
            i++;
        }
        {   // I suppose this is some kind of trick to clean up and restore
            // tracks if track writing raises an exception. 
            auto vr0 = valueRestorer(mOutputTrack[0], outputTrack[0].get());
            auto vr1 = valueRestorer(mOutputTrack[1], outputTrack[1].get());
            if (!nyx_pump_audio(snd, maxdur,
                                &StaticPutCallback, (void*)this)) {
                error = "Error: Something went wrong in evaluating expression.";
            }
        }
        for (int i = 0; i < trkchans; i++) {
            outputTrack[i]->Flush();
            mOutputTime = outputTrack[i]->GetEndTime();

            if (mOutputTime <= 0) {
                error = "Nyquist returned nil audio.\n";
                return;
            }
        }

        for (size_t i = 0; i < trkchans; i++) {
            WaveTrack* out;

            if (chans == trkchans) {
                out = outputTrack[i].get();
            } else {
                out = outputTrack[0].get();
            }

            // See Nyquist.cpp for some code to compute mRestoreSplits and 
            // bMergeClips options.  For now, we ignore options. Let's see 
            // if the behavior is reasonable, and if not we'll try something
            // else.
            mCurTrack[i]->ClearAndPaste(start, start + mOutputTime, out, 
                                   true, true); // mRestoreSplits, bMergeClips
            // See Nyquist.cpp for code dealing with SyncLockGroups here
        }
        // Well, we wrote to the project, so probably something should be 
        // notified. Nyquist.cpp sets mProjectChanged = true; and we're in
        // an effect, but how can we find it? Should this go through globals
        // in nyx.c? Which already uses some globals to negotiate between
        // Audacity and Nyquist?
        // mProjectChanged = true;
    }

    static int StaticPutCallback(float* buffer, int channel,
        int64_t start, int64_t len, int64_t totlen, void* userdata) {
        WaveTrackWriter* wtw = (WaveTrackWriter*)userdata;
        return wtw->PutCallback(buffer, channel, start, len, totlen);
    }

    int PutCallback(float* buffer, int channel,
        int64_t start, int64_t len, int64_t totlen) {
        // Don't let C++ exceptions propagate through the Nyquist library
        return GuardedCall<int>([&] {
             mOutputTrack[channel]->Append((samplePtr)buffer, floatSample, len);

            return 0; // success
            }, MakeSimpleGuard(-1)); // translate all exceptions into failure
    }
};

LVAL putAudio(LVAL nameOrNumber, LVAL expr, double start, double maxdur)
{
    LVAL snd;
    double srate;
    const Track* track;
    xlsave1(snd);
    snd = xleval(expr);
    int nchans = nyx_get_num_channels(snd);
    if (nchans > 2) {
        nyx_xlerror("AUD_PUT_AUDIO got more than 2 channels.");
        goto exit;
    } else if (nchans < 1) {
        nyx_xlerror("AUD_PUT_AUDIO did not get a sound to put.");
        goto exit;
    }
    srate = nyx_get_sample_rate(snd);

    track = getTrack(nameOrNumber);
    if (track) {
        track->TypeSwitch([&](const WaveTrack* waveTrack) {
            auto channels = TrackList::Channels(waveTrack);
            int trkchans = channels.size();  // channels->size();
            double trkrate = waveTrack->GetRate();
            // int64_t len = (int64_t)(maxdur * trkrate + 0.5);
            // allow mono expanded to N trkchans, otherwise require a match
            if (trkchans != nchans) {
                nyx_xlerror("Error: channel count mismatch.");
            } else if (nchans == 0) {
                nyx_xlerror(
                    "Error: sound expression did not return a sound.");
            } else if (nchans == -1) {
                nyx_xlerror(
                    "Error: sound expression returned array with one sound.");
            } else if (trkrate != srate) {
                nyx_xlerror("Error: sample rate mismatch.");
            } else {
                WaveTrackWriter waveTrackWriter(channels, snd, trkrate, 
                                       trkchans, nchans, start, maxdur);
                if (waveTrackWriter.error) {
                    nyx_xlerror(waveTrackWriter.error);
                } else {
                    snd = s_true;
                }
            }
        });
    }
  exit:
    xlpop();
    return snd;
}



LVAL getLabels(LVAL nameOrNumber, double start, double dur)
{
    LVAL labelList, elem;
    xlsave1(labelList);
    xlsave1(elem);  // each sublist ((T0 T1 title))
    LVAL last = NULL;  // last element in labelList
    const Track* track = getTrack(nameOrNumber);
    if (track) {
        track->TypeSwitch([&](const LabelTrack* labelTrack) {
            // int n = labelTrack->GetNumLabels();
            for (const auto& label : labelTrack->GetLabels()) {
                if (label.getT0() >= start &&
                    label.getT0() < start + dur) {
                    elem = consa(cvstring(label.title));
                    elem = cons(cvflonum(label.getT1()), elem);
                    elem = cons(cvflonum(label.getT0()), elem);
                    elem = consa(elem);  // ((T0 T1 title))
                    if (last) {
                        rplacd(last, elem);
                        last = elem;
                    }
                    else {
                        labelList = last = elem;
                    }
                }
            }
        });
    }
    // if not a LabelTrack, fall through, return NULL
    xlpopn(2);
    return labelList;

}


// get a double from either a fixnum or flonum, o.w. return false
bool lvalNum(LVAL lval, double *time)
{
    if (fixp(lval)) {
        *time = getfixnum(lval);
    }  else if (floatp(lval)) {
        *time = getflonum(lval);
    } else return false;
    return true;
}


LVAL putLabels(LVAL nameOrNumber, LVAL labels, LVAL merge_flag)
{
    const Track* track = getTrack(nameOrNumber);
    if (track) {
        track->TypeSwitch([&](const LabelTrack* labelTrack) {
            if (!merge_flag) {
                double start = labelTrack->GetStartTime();
                double end = labelTrack->GetEndTime();
                ((LabelTrack *) labelTrack)->Clear(start - 1, end + 1);
            }
            while (consp(labels)) {
                LVAL triple = car(labels);
                double t0, t1;
                char* label;
                if (!consp(triple) || !lvalNum(car(triple), &t0) ||
                    !consp(cdr(triple)) || !lvalNum(car(cdr(triple)), &t1)) {
                    break;
                }
                triple = cdr(cdr(triple));  // move to string
                if (!consp(triple) || !stringp(car(triple))) {
                    break;
                }
                label = (char*)getstring(car(triple));
                SelectedRegion region(t0, t1);
                // insert the label into the track
                wxString labelString(label);
                ((LabelTrack *) labelTrack)->AddLabel(region, labelString);
                labels = cdr(labels);
            }
            // if we encounter an error, we exit the loop, leaving labels
            // pointing near the error we encountered.
            if (labels) {
                xlerror("not a valid label list", labels);
            }
        });
    }

    return labels;
}


// appends attribute and value to list, which is the last
// cons cell in a NOTE or UPDATE so far. The 
// new last cons cell containing the value is returned.
LVAL appendParameter(LVAL list, Alg_parameter_ptr param, LVAL STRUE)
{
    char name[64];
    // allow attributes and atom values to have length up to 60:
    rplacd(list, consa(NIL));  // append attr to list
    list = cdr(list);
    if (strlen(param->attr_name()) <= 60) {
        // convert to upper case:
        strcpy(name, param->attr_name());
        for (char* ptr = name; *ptr; ptr++) {
            *ptr = toupper(*ptr);
        }
        rplaca(list, xlenter(name));
    } // otherwise attribute is left as NIL
    rplacd(list, consa(NIL));  // append value to list
    list = cdr(list);
    switch (param->attr_type()) {
    case 'i':
        rplaca(list, cvfixnum(param->i));
        break;
    case 'r':
        rplaca(list, cvflonum(param->r));
        break;
    case 's':
        rplaca(list, cvstring(param->s));
        break;
    case 'l':
        rplaca(list, param->l ? STRUE : NIL);
        break;
    case 'a':
        // atoms are stored like attributes as if the last character is a
        // type character which is at param->a[0]. Skip the type character:
        rplaca(list, xlenter(param->a + 1));
        break;
    default: /// perhaps we should report an error. This should never happen.
        break;
    }
    return list;
}


// return the last cons cell in noteList after appending an update
LVAL appendUpdate(LVAL *noteList, LVAL last, Alg_event_ptr ev,
    LVAL SUPDATE, LVAL STRUE)
{
    Alg_update_ptr update = (Alg_update_ptr)ev;
    // append a cons at the top-level
    if (*noteList) {
        rplacd(last, consa(NIL));
        last = cdr(last);
    } else {
        *noteList = consa(NIL);
        last = *noteList;
    }
    // last is now a cons where the update goes
    LVAL list = consa(SUPDATE);
    rplaca(last, list);
    rplacd(list, consa(NIL));  // append key
    list = cdr(list);
    rplaca(list, cvfixnum(update->get_identifier()));
    rplacd(list, consa(NIL));  // append time
    list = cdr(list);
    rplaca(list, cvflonum(update->time));
    rplacd(list, consa(NIL));  // append channel 
    list = cdr(list);
    rplaca(list, cvfixnum(update->chan));
    // append attribute and value:
    appendParameter(list, &update->parameter, STRUE);
    return last;
}


LVAL appendNote(LVAL *noteList, LVAL last, Alg_event_ptr ev, 
                LVAL SNOTE, LVAL STRUE)
{
    Alg_note_ptr note = (Alg_note_ptr) ev;
    // append a cons at the top-level
    if (*noteList) {
        rplacd(last, consa(NIL));
        last = cdr(last);
    } else {
        *noteList = consa(NIL);
        last = *noteList;
    }
    // last is now a cons where the note goes
    LVAL list = consa(SNOTE);
    rplaca(last, list);
    rplacd(list, consa(NIL));  // append key
    list = cdr(list);
    rplaca(list, cvfixnum(note->get_identifier()));
    rplacd(list, consa(NIL));  // append time
    list = cdr(list);
    rplaca(list, cvflonum(note->time));
    rplacd(list, consa(NIL));  // append channel 
    list = cdr(list);
    rplaca(list, cvfixnum(note->chan));
    rplacd(list, consa(NIL));  // append pitch 
    list = cdr(list);
    rplaca(list, cvflonum(note->get_pitch()));
    rplacd(list, consa(NIL));  // append loudness 
    list = cdr(list);
    rplaca(list, cvflonum(note->get_loud()));
    rplacd(list, consa(NIL));  // append duration 
    list = cdr(list);
    rplaca(list, cvflonum(note->get_duration()));
    for (Alg_parameters_ptr params = note->parameters; params; params->next) {
        list = appendParameter(list, &params->parm, STRUE);
    }
    return last;
}

LVAL getNotes(LVAL nameOrNumber, double start, double dur, LVAL inbeats)
{
    LVAL noteList;
    xlsave1(noteList);
    LVAL last = NULL;  // last element in noteList
    const Track* track = getTrack(nameOrNumber);
    LVAL SNOTE = xlenter("NOTE");
    LVAL SUPDATE = xlenter("UPDATE");
    LVAL STRUE = xlenter("T");
    if (track) {
        track->TypeSwitch([&](const NoteTrack* noteTrack) {
            Alg_seq& seq = noteTrack->GetSeq();
            Alg_iterator iter(&seq, false);
            iter.begin();
            Alg_event_ptr event;
            while (event = iter.next(NULL, NULL, NULL, dur)) {
                if (event->time < start) {
                    continue;  // skip to start time
                }
                if (event->is_note()) {
                    last = appendNote(&noteList, last, event, SNOTE, STRUE);
                } else if (event->is_update()) {
                    last = appendUpdate(&noteList, last, event, SUPDATE, STRUE);
                }
            }
            iter.end();
            });
    }
    xlpop();
    return noteList;
}

// extract an attribute and value from plist, set attribute in event,
//    which may be an Alg_note or Alg_update.
// prerequisite: consp(plist)
// return cdr(cdr(plist)) if successful; otherwise return plist
//    note that return value NULL indicates success and there are
//    no more properties to parse.
//
LVAL getParameter(Alg_event_ptr event, LVAL plist, LVAL STRUE)
{
    if (!symbolp(car(plist))) {
        return NULL;
    }
    char* symname = (char *) getstring(getpname(car(plist)));
    if (symname[0] == ':') {
        symname++;  // trim the colon if it is there
    }
    if (strlen(symname) > 60) {
        return plist;
    }
    char attr[64];
    strcpy(attr, symname);
    // to lower case:
    for (char* ptr = attr; *ptr; ptr++) {
        *ptr = tolower(*ptr);
    }
    // Allegro attributes have a type character at the end,
    // e.g. bendi is an integer attribute.
    char typechar = attr[strlen(attr) - 1];
    if (!consp(cdr(plist))) {
        return NULL;
    }
    LVAL val = car(cdr(plist));
    if (fixp(val) && typechar == 'i') {
        event->set_integer_value(attr, getfixnum(val));
    } else if (floatp(val) && typechar == 'r') {
        event->set_real_value(attr, getflonum(val));
    } else if (stringp(val) && typechar == 's') {
        event->set_string_value(attr,
            (char*)getstring(val));
    } else if (symbolp(val) && typechar == 'a') {
        // note: atom names are not required to end in a type character
        // atoms are stored without case conversion, so typically 
        // upper case as in XLisp symbols.
        Alg_attribute a = symbol_table.insert_string(
                              (const char *) getstring(getpname(val)));
        event->set_atom_value(attr, a);
    } else if (val == NULL ||
        val == STRUE && typechar == 'l') {
        event->set_logical_value(attr, (val == STRUE));
    } else {
        return NULL;
    }
    return cdr(cdr(plist));
}


LVAL putNotes(LVAL nameOrNumber, LVAL notes, LVAL inbeats, LVAL merge_flag)
{
    const Track* track = getTrack(nameOrNumber);
    if (track) {
        track->TypeSwitch([&](const NoteTrack* noteTrack) {
            Alg_seq& seq = noteTrack->GetSeq();
            if (inbeats) {
                seq.convert_to_beats();
            } else {
                seq.convert_to_seconds();
            }
            if (!merge_flag) { // free notes in track
                // we could delete the whole seq, but we will keep tempo
                // information and just delete all the events:
                for (int j = 0; j < seq.track_list.length(); j++) {
                    Alg_track& notes = seq.track_list[j];
                    // Alg_events does not delete notes 
                    for (int i = 0; i < notes.length(); i++) {
                        Alg_event_ptr event = notes[i];
                        delete event;
                    }
                }
                seq.track_list.reset();
                seq.track_list.add_track(0, seq.get_time_map(), 
                                         seq.get_units_are_seconds());
            }
            LVAL SNOTE = xlenter("NOTE");
            LVAL SUPDATE = xlenter("UPDATE");
            LVAL STRUE = xlenter("T");
            // insert notes from list
            Alg_note_ptr anote = NULL;
            Alg_update_ptr update = NULL;
            while (consp(notes)) {
                LVAL note = car(notes);
                if (!consp(note)) {
                    break;
                }
                if (car(note) == SNOTE) {
                    note = cdr(note);  // get the key (id)
                    if (!consp(note) || !fixp(car(note))) {
                        break;
                    }
                    anote = new Alg_note();
                    anote->set_identifier(getfixnum(car(note)));

                    note = cdr(note);  // get the time
                    if (!consp(note) || !lvalNum(car(note), &anote->time)) {
                        break;
                    }

                    note = cdr(note);  // get the channel
                    if (!consp(note) || !fixp(car(note))) {
                        break;
                    }
                    anote->chan = getfixnum(car(note));

                    note = cdr(note);  // get the pitch
                    double pitch;
                    if (!consp(note) || !lvalNum(car(note), &pitch)) {
                        break;
                    }
                    anote->set_pitch(pitch);

                    note = cdr(note);  // get the loudness
                    double loudness;
                    if (!consp(note) || !lvalNum(car(note), &loudness)) {
                        break;
                    }
                    anote->set_loud(loudness);

                    note = cdr(note);  // get duration
                    if (!consp(note) || !lvalNum(car(note), &anote->dur)) {
                        break;
                    }
                    note = cdr(note);

                    while (consp(note)) { // get attribute/value pairs
                        LVAL next = getParameter(anote, note, STRUE);
                        if (next == note) {  // error encountered
                            break;
                        }
                        note = next;  // skip to next attr/value pair
                    }
                } else if (car(note) == SUPDATE) {
                    note = cdr(note);
                    if (!consp(note) || !fixp(car(note))) {
                        break;
                    }
                    update = new Alg_update();  // get key (id)
                    update->set_identifier(getfixnum(car(note)));

                    note = cdr(note);  // get the time
                    if (!consp(note) || !lvalNum(car(note), &anote->time)) {
                        break;
                    }

                    note = cdr(note);  // get the channel
                    if (!consp(note) || !fixp(car(note))) {
                        break;
                    }
                    anote->chan = getfixnum(car(note));

                    note = cdr(note);  // get parameter
                    if (!consp(note)) {
                        break;
                    }
                    LVAL next = getParameter(update, note, STRUE);
                    if (next) {  // there should be 1 attr/val pair, so next
                        break;   // should be NULL. Otherwise, error.
                    }
                }
                if (note) {
                    break;
                } else { // good, used all list values
                    seq.add_event(anote, 0);
                    anote = NULL;
                }
                notes = cdr(notes);
            }
            if (notes) { // something went wrong
                if (anote) {
                    delete anote; // didn't use it in note track
                }
                if (update) {
                    delete update;  // didn't use it
                }
                xlerror("Invalid list of notes", notes);
            }
            // DEBUG: Write the track
            seq.write("seq-after-aud-put.gio");
        });
    }
    return notes;
}


LVAL getTimes(LVAL nameOrNumber, double start, double dur)
{
    return NULL;
}


LVAL putTimes(LVAL nameOrNumber, LVAL breakpoints)
{
    return NULL;
}


// call this when effect has completed
void nyquistAPICleanup(void)
{
    waveTrackReaders.clear();
}

