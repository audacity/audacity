/**********************************************************************

  Audacity: A Digital Audio Editor

  NoteTrack.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_NOTETRACK__
#define __AUDACITY_NOTETRACK__

#include <wx/string.h>
#include "Audacity.h"
#include "Experimental.h"
#include "Track.h"

#if defined(USE_MIDI)

class wxDC;
class wxRect;

class DirManager;
class Alg_seq;   // from "allegro.h"

class AUDACITY_DLL_API NoteTrack:public Track {
 public:
   friend class TrackArtist;

   NoteTrack(DirManager * projDirManager);
   virtual ~NoteTrack();

   virtual Track *Duplicate();
   
   virtual int GetKind() const { return Note; } 

   virtual double GetStartTime() { return 0.0; }
   virtual double GetEndTime() { return mLen; }

   void DrawLabelControls(wxDC & dc, wxRect & r);
   bool LabelClick(wxRect & r, int x, int y, bool right);

   void SetSequence(Alg_seq *seq);
   Alg_seq* GetSequence();
   void PrintSequence();

   int GetVisibleChannels();

   bool ExportMIDI(wxString f);
   bool ExportAllegro(wxString f);

/* REQUIRES PORTMIDI */
//   int GetLastMidiPosition() const { return mLastMidiPosition; }
//   void SetLastMidiPosition( int position )
//   {
//      mLastMidiPosition = position;
//   }

   // High-level editing
   virtual bool Cut  (double t0, double t1, Track **dest);
   virtual bool Copy (double t0, double t1, Track **dest);
   virtual bool Clear(double t0, double t1);
   virtual bool Paste(double t, Track *src);

   int GetBottomNote() const { return mBottomNote; }
   void SetBottomNote(int note) 
   { 
      if (note < 0)
         note = 0;
      else if (note > 96)
         note = 96;

      mBottomNote = note; 
   }

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

 private:
   Alg_seq *mSeq; // NULL means no sequence
   // when Duplicate() is called, assume that it is to put a copy
   // of the track into the undo stack or to redo/copy from the
   // stack to the project object. We want copies to the stack
   // to be serialized (therefore compact) representations, so
   // copy will set mSeq to NULL and serialize to the following
   // variables. If this design is correct, the track will be
   // duplicated again (in the event of redo) back to the project
   // at which point we will unserialize the data back to the
   // mSeq variable. (TrackArtist should check to make sure this
   // flip-flop from mSeq to mSerializationBuffer happened an
   // even number of times, otherwise mSeq will be NULL).
   void *mSerializationBuffer; // NULL means no buffer
   long mSerializationLength;
   double mLen;

   DirManager *mDirManager;

   int mBottomNote;

   int mVisibleChannels;
   int mLastMidiPosition;
};

#endif // USE_MIDI

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 214ce825-eb40-416f-9312-84652d6025d1

