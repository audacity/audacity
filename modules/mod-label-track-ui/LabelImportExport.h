/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LabelImportExport.h
  @brief Define legible text format for saving and loading labels

  Paul Licameli split from LabelTrack.h

**********************************************************************/

#ifndef __AUDACITY_LABEL_IMPORT_EXPORT__
#define __AUDACITY_LABEL_IMPORT_EXPORT__

class LabelStruct;
class LabelTrack;
class wxTextFile;

LabelStruct ImportLabelStruct(wxTextFile &file, int &index);
void ExportLabelStruct(const LabelStruct &label, wxTextFile &file);
void ImportLabelTrack(LabelTrack &track, wxTextFile & f);
void ExportLabelTrack(const LabelTrack &track, wxTextFile & f);

#endif
