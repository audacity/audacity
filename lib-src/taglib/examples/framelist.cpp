/* Copyright (C) 2003 Scott Wheeler <wheeler@kde.org>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <iostream>
#include <stdlib.h>

#include <tbytevector.h>

#include <mpegfile.h>

#include <id3v2tag.h>
#include <id3v2frame.h>
#include <id3v2header.h>

#include <id3v1tag.h>

#include <apetag.h>

using namespace std;
using namespace TagLib;

int main(int argc, char *argv[])
{
  // process the command line args


  for(int i = 1; i < argc; i++) {

    cout << "******************** \"" << argv[i] << "\"********************" << endl;

    MPEG::File f(argv[i]);

    ID3v2::Tag *id3v2tag = f.ID3v2Tag();

    if(id3v2tag) {

      cout << "ID3v2."
           << id3v2tag->header()->majorVersion()
           << "."
           << id3v2tag->header()->revisionNumber()
           << ", "
           << id3v2tag->header()->tagSize()
           << " bytes in tag"
           << endl;

      ID3v2::FrameList::ConstIterator it = id3v2tag->frameList().begin();
      for(; it != id3v2tag->frameList().end(); it++)
        cout << (*it)->frameID() << " - \"" << (*it)->toString() << "\"" << endl;
    }
    else
      cout << "file does not have a valid id3v2 tag" << endl;

    cout << endl << "ID3v1" << endl;

    ID3v1::Tag *id3v1tag = f.ID3v1Tag();

    if(id3v1tag) {
      cout << "title   - \"" << id3v1tag->title()   << "\"" << endl;
      cout << "artist  - \"" << id3v1tag->artist()  << "\"" << endl;
      cout << "album   - \"" << id3v1tag->album()   << "\"" << endl;
      cout << "year    - \"" << id3v1tag->year()    << "\"" << endl;
      cout << "comment - \"" << id3v1tag->comment() << "\"" << endl;
      cout << "track   - \"" << id3v1tag->track()   << "\"" << endl;
      cout << "genre   - \"" << id3v1tag->genre()   << "\"" << endl;
    }
    else
      cout << "file does not have a valid id3v1 tag" << endl;

    APE::Tag *ape = f.APETag();

    cout << endl << "APE" << endl;

    if(ape) {
      for(APE::ItemListMap::ConstIterator it = ape->itemListMap().begin();
          it != ape->itemListMap().end(); ++it)
      {
        if((*it).second.type() != APE::Item::Binary)
          cout << (*it).first << " - \"" << (*it).second.toString() << "\"" << endl;
        else
          cout << (*it).first << " - Binary data (" << (*it).second.binaryData().size() << " bytes)" << endl;
      }
    }
    else
      cout << "file does not have a valid APE tag" << endl;

    cout << endl;
  }
}
