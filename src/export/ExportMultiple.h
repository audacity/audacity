/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMultiple.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT_MULTIPLE__
#define __AUDACITY_EXPORT_MULTIPLE__

#include <wx/dialog.h>
#include <wx/string.h>
#include <wx/dynarray.h>   // sadly we are using wx dynamic arrays
#include <wx/listctrl.h>
#include <wx/simplebook.h>

#include "Export.h"
#include "../Tags.h"       // we need to know about the Tags class for metadata

class wxButton;
class wxCheckBox;
class wxChoice;
class wxRadioButton;
class wxTextCtrl;

class AudacityProject;
class LabelTrack;
class ShuttleGui;
class Track;

class ExportMultiple final : public wxDialogWrapper
{
public:

   ExportMultiple(AudacityProject *parent);
   virtual ~ExportMultiple();

   int ShowModal();

private:

   // Export
   void CanExport();
   void CountTracksAndLabels();
   bool DirOk();
   /** \brief Export multiple labeled regions of the project to separate files
    *
    * Uses a single label track in the project to split up the audio into a
    * series of sections, each of which is exported to a separate file.
    * @param byName Controls whether files are named after the text in the
    * labels that define them (true), or just numbered (false).
    * @param prefix The string used to prefix the file number if files are being
    * numbered rather than named */
   int ExportMultipleByLabel(bool byName, const wxString &prefix, bool addNumber);

   /** \brief Export each track in the project to a separate file
    *
    * @param byName Controls whether files are named after the track names
    * (true), or just numbered (false).
    * @param prefix The string used to prefix the file number if files are being
    * numbered rather than named */
   int ExportMultipleByTrack(bool byName, const wxString &prefix, bool addNumber);

   /** Export one file of an export multiple set
    *
    * Called once for each file in the list to do a (non-interactive) export
    * @param channels Number of channels to export
    * @param name The file name (and path) to export to
    * @param selectedOnly Should we  export the selected tracks only?
    * @param t0 Start time for export
    * @param t1 End time for export
    * @param tags Metadata to include in the file (if possible).
    */
   int DoExport(unsigned channels,
                 const wxFileName &name,
                 bool selectedOnly,
                 double t0,
                 double t1,
                 const Tags &tags);
   /** \brief Takes an arbitrary text string and converts it to a form that can
    * be used as a file name, if necessary prompting the user to edit the file
    * name produced */
   wxString MakeFileName(const wxString &input);
   // Dialog
   void PopulateOrExchange(ShuttleGui& S);
   void EnableControls();

   void OnFormat(wxCommandEvent& event);
   void OnOptions(wxCommandEvent& event);
   void OnCreate(wxCommandEvent& event);
   void OnChoose(wxCommandEvent& event);
   void OnLabel(wxCommandEvent& event);
   void OnFirst(wxCommandEvent& event);
   void OnFirstFileName(wxCommandEvent& event);
   void OnTrack(wxCommandEvent& event);
   void OnByName(wxCommandEvent& event);
   void OnByNumber(wxCommandEvent& event);
   void OnPrefix(wxCommandEvent& event);
   void OnCancel(wxCommandEvent& event);
   void OnExport(wxCommandEvent& event);

private:
   Exporter mExporter;
   std::vector<ExportPlugin*> mPlugins;   /**< Array of references to available exporter
                                   plug-ins */
   AudacityProject *mProject;
   TrackList *mTracks;           /**< The list of tracks in the project that is
                                   being exported */
   LabelTrack *mLabels;
   int mNumLabels;
   int mNumWaveTracks;

   // PRL:  This is never populated anywhere?
   std::vector<Track*> mSelected;

   int mFilterIndex;          /**< The index in the drop-down list of export
                                formats (mFormat) of the selected export format.
                                This list includes all possible
                                plug-in - subformat combinations. */
   int mPluginIndex;          /**< The index in mPlugins of the selected export
                              plug-in */
   int mSubFormatIndex;       /**< The selected subformat number within the
                                selected export plug-in set by mPluginIndex */
   bool mInitialized;

   // List of file actually exported
   wxArrayString mExported;

   wxChoice      *mFormat;    /**< Drop-down list of export formats
                                (combinations of plug-in and subformat) */
   wxButton      *mOptions;

   wxTextCtrl    *mDir;    /**< The directory all the exported files will end
                             up in */
   wxButton      *mCreate;
   wxButton      *mChoose;

   wxRadioButton *mLabel;  /**< button to choose export based on Labels */
   wxStaticText  *mLabelLabel;   /**< description text for mLabel */

   wxCheckBox    *mFirst;  /**< Check box to export audio before first label */
   wxStaticText  *mFirstFileLabel;  /**< description text for mFirstFileName */
   wxTextCtrl    *mFirstFileName;   /**< Name to use for exporting audio before
                                      the first label in the file */

   wxRadioButton *mTrack;  /**< button to choose export based on tracks */
   wxStaticText  *mTrackLabel;

   wxRadioButton *mByNumberAndName; /**< button to choose number AND name for exported files */
   wxRadioButton *mByName;    /**< button to choose naming exported file from label text */
   wxRadioButton *mByNumber;  /**< button to choose numbering exported files */

   wxStaticText  *mPrefixLabel;
   wxTextCtrl    *mPrefix;

   wxCheckBox    *mOverwrite;

   wxButton      *mCancel;
   wxButton      *mExport;

   wxSimplebook   *mBook;

   DECLARE_EVENT_TABLE()

};

class SuccessDialog final : public wxDialogWrapper
{
public:
   SuccessDialog(wxWindow *parent, wxWindowID id, const wxString &title) :
      wxDialogWrapper(parent, id, title, wxDefaultPosition,
         wxDefaultSize, wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER) {};
   void OnKeyDown(wxListEvent& event); // dismisses dialog when <enter> is pressed with list control having focus
   void OnItemActivated(wxListEvent& event); // dismisses dialog when <enter> is pressed with list item having focus
private:
   DECLARE_EVENT_TABLE()
};

class MouseEvtHandler final : public wxEvtHandler
{
public:
   void OnMouse(wxMouseEvent& event);
private:
   DECLARE_EVENT_TABLE()
};


/** \brief A private class used to store the information needed to do an
    * export.
    *
    * We create a set of these during the interactive phase of the export
    * cycle, then use them when the actual exports are done. */
   class ExportKit
   {
   public:
      Tags filetags; /**< The set of metadata to use for the export */
      wxFileName destfile; /**< The file to export to */
      double t0;           /**< Start time for the export */
      double t1;           /**< End time for the export */
      unsigned channels;   /**< Number of channels for ExportMultipleByTrack */
   };  // end of ExportKit declaration
   /* we are going to want an set of these kits, and don't know how many until
    * runtime. I would dearly like to use a std::vector, but it seems that
    * this isn't done anywhere else in Audacity, presumably for a reason?, so
    * I'm stuck with wxArrays, which are much harder, as well as non-standard.
    */
   WX_DECLARE_OBJARRAY(ExportKit, ExportKitArray);


#endif
