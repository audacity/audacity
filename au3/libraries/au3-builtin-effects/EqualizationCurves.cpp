/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationCurves.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

**********************************************************************/

#include "EqualizationCurves.h"
#include "EqualizationParameters.h"

#include <wx/log.h>
#include "FileNames.h"
#include "XMLFileReader.h"

// Increment whenever EQCurves.xml is updated
#define EQCURVES_VERSION   1
#define EQCURVES_REVISION  0
#define UPDATE_ALL 0 // 0 = merge NEW presets only, 1 = Update all factory presets.

wxString EQCurveReader::GetPrefsPrefix()
{
    wxString base = wxT("/Effects/Equalization/");
    if (mOptions == kEqOptionGraphic) {
        base = wxT("/Effects/GraphicEq/");
    } else if (mOptions == kEqOptionCurve) {
        base = wxT("/Effects/FilterCurve/");
    }
    return base;
}

//
// Load external curves with fallback to default, then message
//
void EQCurveReader::LoadCurves(const wxString& fileName, bool append)
{
// We've disabled the XML management of curves.
// Just going via .cfg files now.
#ifndef LEGACY_EQ
    (void)fileName;
    (void)append;
    mCurves.clear();
    mCurves.push_back(wxT("unnamed"));    // we still need a default curve to use
#else
    // Construct normal curve filename
    //
    // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
    //       between wxStandardPaths and wxConfig under Linux.  The latter
    //       creates a normal file as "$HOME/.audacity", while the former
    //       expects the ".audacity" portion to be a directory.
    // MJS:  I don't know what the above means, or if I have broken it.
    wxFileName fn;

    if (fileName.empty()) {
        // Check if presets are up to date.
        wxString eqCurvesCurrentVersion = wxString::Format(wxT("%d.%d"), EQCURVES_VERSION, EQCURVES_REVISION);
        wxString eqCurvesInstalledVersion;
        gPrefs->Read(GetPrefsPrefix() + "PresetVersion", &eqCurvesInstalledVersion, wxT(""));

        bool needUpdate = (eqCurvesCurrentVersion != eqCurvesInstalledVersion);

        // UpdateDefaultCurves allows us to import NEW factory presets only,
        // or update all factory preset curves.
        if (needUpdate) {
            UpdateDefaultCurves(UPDATE_ALL != 0);
        }
        fn = wxFileName(FileNames::DataDir(), wxT("EQCurves.xml"));
    } else {
        fn = fileName; // user is loading a specific set of curves
    }
    // If requested file doesn't exist...
    if (!fn.FileExists() && !GetDefaultFileName(fn)) {
        mCurves.clear();
        /* i18n-hint: name of the 'unnamed' custom curve */
        mCurves.push_back(_("unnamed"));   // we still need a default curve to use
        return;
    }

    EQCurve tempCustom(wxT("temp"));
    if (append == false) { // Start from scratch
        mCurves.clear();
    } else { // appending so copy and remove 'unnamed', to replace later
        tempCustom.points = mCurves.back().points;
        mCurves.pop_back();
    }

    // Load the curves
    XMLFileReader reader;
    const wxString fullPath{ fn.GetFullPath() };
    if (!reader.Parse(this, fullPath)) {
        /* i18n-hint: EQ stands for 'Equalization'.*/
        auto msg = XO("Error Loading EQ Curves from file:\n%s\nError message says:\n%s")
                   .Format(fullPath, reader.GetErrorStr());
        // Inform user of load failure
        using namespace BasicUI;
        ShowMessageBox(msg, MessageBoxOptions {}.IconStyle(Icon::Error));
        mCurves.push_back(_("unnamed"));  // we always need a default curve to use
        return;
    }

    // Move "unnamed" to end, if it exists in current language.
    int numCurves = mCurves.size();
    int curve;
    EQCurve tempUnnamed(wxT("tempUnnamed"));
    for ( curve = 0; curve < numCurves - 1; curve++ ) {
        if (mCurves[curve].Name == _("unnamed")) {
            tempUnnamed.points = mCurves[curve].points;
            mCurves.erase(mCurves.begin() + curve);
            mCurves.push_back(_("unnamed"));  // add 'unnamed' back at the end
            mCurves.back().points = tempUnnamed.points;
        }
    }

    if (mCurves.back().Name != _("unnamed")) {
        mCurves.push_back(_("unnamed"));   // we always need a default curve to use
    }
    if (append == true) {
        mCurves.back().points = tempCustom.points;
    }
#endif
    return;
}

//
// Update presets to match Audacity version.
//
void EQCurveReader::UpdateDefaultCurves(bool updateAll /* false */)
{
    if (mCurves.size() == 0) {
        return;
    }

    wxString unnamed = wxT("unnamed");

    // Save the "unnamed" curve and remove it so we can add it back as the final curve.
    EQCurve userUnnamed(wxT("temp"));
    userUnnamed = mCurves.back();
    mCurves.pop_back();

    EQCurveArray userCurves = mCurves;
    mCurves.clear();
    // We only wamt to look for the shipped EQDefaultCurves.xml
    wxFileName fn = wxFileName(FileNames::ResourcesDir(), wxT("EQDefaultCurves.xml"));
    wxLogDebug(wxT("Attempting to load EQDefaultCurves.xml from %s"), fn.GetFullPath());
    XMLFileReader reader;

    if (!reader.Parse(this, fn.GetFullPath())) {
        wxLogError(wxT("EQDefaultCurves.xml could not be read."));
        return;
    } else {
        wxLogDebug(wxT("Loading EQDefaultCurves.xml successful."));
    }

    EQCurveArray defaultCurves = mCurves;
    mCurves.clear(); // clear now so that we can sort then add back.

    // Remove "unnamed" if it exists.
    if (defaultCurves.back().Name == unnamed) {
        defaultCurves.pop_back();
    } else {
        wxLogError(wxT("Error in EQDefaultCurves.xml"));
    }

    int numUserCurves = userCurves.size();
    int numDefaultCurves = defaultCurves.size();
    EQCurve tempCurve(wxT("test"));

    if (updateAll) {
        // Update all factory preset curves.
        // Sort and add factory defaults first;
        mCurves = defaultCurves;
        std::sort(mCurves.begin(), mCurves.end());
        // then add remaining user curves:
        for (int curveCount = 0; curveCount < numUserCurves; curveCount++) {
            bool isCustom = true;
            tempCurve = userCurves[curveCount];
            // is the name in the default set?
            for (int defCurveCount = 0; defCurveCount < numDefaultCurves; defCurveCount++) {
                if (tempCurve.Name == mCurves[defCurveCount].Name) {
                    isCustom = false;
                    break;
                }
            }
            // if tempCurve is not in the default set, add it to mCurves.
            if (isCustom) {
                mCurves.push_back(tempCurve);
            }
        }
    } else {
        // Import NEW factory defaults but retain all user modified curves.
        for (int defCurveCount = 0; defCurveCount < numDefaultCurves; defCurveCount++) {
            bool isUserCurve = false;
            // Add if the curve is in the user's set (preserve user's copy)
            for (int userCurveCount = 0; userCurveCount < numUserCurves; userCurveCount++) {
                if (userCurves[userCurveCount].Name == defaultCurves[defCurveCount].Name) {
                    isUserCurve = true;
                    mCurves.push_back(userCurves[userCurveCount]);
                    break;
                }
            }
            if (!isUserCurve) {
                mCurves.push_back(defaultCurves[defCurveCount]);
            }
        }
        std::sort(mCurves.begin(), mCurves.end());
        // now add the rest of the user's curves.
        for (int userCurveCount = 0; userCurveCount < numUserCurves; userCurveCount++) {
            bool isDefaultCurve = false;
            tempCurve = userCurves[userCurveCount];
            for (int defCurveCount = 0; defCurveCount < numDefaultCurves; defCurveCount++) {
                if (tempCurve.Name == defaultCurves[defCurveCount].Name) {
                    isDefaultCurve = true;
                    break;
                }
            }
            if (!isDefaultCurve) {
                mCurves.push_back(tempCurve);
            }
        }
    }
    defaultCurves.clear();
    userCurves.clear();

    // Add back old "unnamed"
    if (userUnnamed.Name == unnamed) {
        mCurves.push_back(userUnnamed);   // we always need a default curve to use
    }

    EQCurveWriter { mCurves }.SaveCurves();

    // Write current EqCurve version number
    // TODO: Probably better if we used pluginregistry.cfg
    wxString eqCurvesCurrentVersion = wxString::Format(wxT("%d.%d"), EQCURVES_VERSION, EQCURVES_REVISION);
    gPrefs->Write(GetPrefsPrefix() + "PresetVersion", eqCurvesCurrentVersion);
    gPrefs->Flush();

    return;
}

//
// Get fully qualified filename of EQDefaultCurves.xml
//
bool EQCurveReader::GetDefaultFileName(wxFileName& fileName)
{
    // look in data dir first, in case the user has their own defaults (maybe downloaded ones)
    fileName = wxFileName(FileNames::DataDir(), wxT("EQDefaultCurves.xml"));
    if (!fileName.FileExists()) { // Default file not found in the data dir.  Fall back to Resources dir.
                                  // See http://docs.wxwidgets.org/trunk/classwx_standard_paths.html#5514bf6288ee9f5a0acaf065762ad95d
        fileName = wxFileName(FileNames::ResourcesDir(), wxT("EQDefaultCurves.xml"));
    }
    if (!fileName.FileExists()) {
        // LLL:  Is there really a need for an error message at all???
        //auto errorMessage = XO("EQCurves.xml and EQDefaultCurves.xml were not found on your system.\nPlease press 'help' to visit the download page.\n\nSave the curves at %s")
        //   .Format( FileNames::DataDir() );
        //BasicUI::ShowErrorDialog( wxWidgetsWindowPlacement{ mUIParent },
        //   XO("EQCurves.xml and EQDefaultCurves.xml missing"),
        //   errorMessage, wxT("http://wiki.audacityteam.org/wiki/EQCurvesDownload"), false);

        // Have another go at finding EQCurves.xml in the data dir, in case 'help' helped
        fileName = wxFileName(FileNames::DataDir(), wxT("EQDefaultCurves.xml"));
    }
    return fileName.FileExists();
}

//
// Save curves to external file
//
void EQCurveWriter::SaveCurves(const wxString& fileName)
{
    wxFileName fn;
    if (fileName.empty()) {
        // Construct default curve filename
        //
        // LLL:  Wouldn't you know that as of WX 2.6.2, there is a conflict
        //       between wxStandardPaths and wxConfig under Linux.  The latter
        //       creates a normal file as "$HOME/.audacity", while the former
        //       expects the ".audacity" portion to be a directory.
        fn = wxFileName(FileNames::DataDir(), wxT("EQCurves.xml"));

        // If the directory doesn't exist...
        if (!fn.DirExists()) {
            // Attempt to create it
            if (!fn.Mkdir(fn.GetPath(), 511, wxPATH_MKDIR_FULL)) {
                // MkDir() will emit message
                return;
            }
        }
    } else {
        fn = fileName;
    }

    GuardedCall([&] {
        // Create/Open the file
        const wxString fullPath { fn.GetFullPath() };
        XMLFileWriter eqFile { fullPath, XO("Error Saving Equalization Curves") };

        // Write the curves
        WriteXML(eqFile);

        eqFile.Commit();
    });
}

//
// Process XML tags and handle the ones we recognize
//
bool EQCurveReader::HandleXMLTag(
    const std::string_view& tag, const AttributesList& attrs)
{
    // May want to add a version strings...
    if (tag == "equalizationeffect") {
        return true;
    }

    // Located a NEW curve
    if (tag == "curve") {
        // Process the attributes
        for (auto pair : attrs) {
            auto attr = pair.first;
            auto value = pair.second;

            // Create a NEW curve and name it
            if (attr == "name") {
                const wxString strValue = value.ToWString();
                // check for a duplicate name and add (n) if there is one
                int n = 0;
                wxString strValueTemp = strValue;
                bool exists;
                do{
                    exists = false;
                    for (size_t i = 0; i < mCurves.size(); i++) {
                        if (n > 0) {
                            strValueTemp.Printf(wxT("%s (%d)"), strValue, n);
                        }
                        if (mCurves[i].Name == strValueTemp) {
                            exists = true;
                            break;
                        }
                    }
                    n++;
                }while (exists == true);

                mCurves.push_back(EQCurve(strValueTemp));
            }
        }

        // Tell caller it was processed
        return true;
    }

    // Located a NEW point
    if (tag == "point") {
        // Set defaults in case attributes are missing
        double f = 0.0;
        double d = 0.0;

        // Process the attributes
        double dblValue;
        for (auto pair : attrs) {
            auto attr = pair.first;
            auto value = pair.second;

            // Get the frequency
            if (attr == "f") {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                f = dblValue;
            }
            // Get the dB
            else if (attr == "d") {
                if (!value.TryGet(dblValue)) {
                    return false;
                }
                d = dblValue;
            }
        }

        // Create a NEW point
        mCurves[ mCurves.size() - 1 ].points.push_back(EQPoint(f, d));

        // Tell caller it was processed
        return true;
    }

    // Tell caller we didn't understand the tag
    return false;
}

//
// Return handler for recognized tags
//
XMLTagHandler* EQCurveReader::HandleXMLChild(const std::string_view& tag)
{
    if (tag == "equalizationeffect") {
        return this;
    }

    if (tag == "curve") {
        return this;
    }

    if (tag == "point") {
        return this;
    }

    return NULL;
}

//
// Write all of the curves to the XML file
//
void EQCurveWriter::WriteXML(XMLWriter& xmlFile) const
// may throw
{
    // Start our hierarchy
    xmlFile.StartTag(wxT("equalizationeffect"));

    // Write all curves
    int numCurves = mCurves.size();
    int curve;
    for ( curve = 0; curve < numCurves; curve++ ) {
        // Start a NEW curve
        xmlFile.StartTag(wxT("curve"));
        xmlFile.WriteAttr(wxT("name"), mCurves[ curve ].Name);

        // Write all points
        int numPoints = mCurves[ curve ].points.size();
        int point;
        for ( point = 0; point < numPoints; point++ ) {
            // Write NEW point
            xmlFile.StartTag(wxT("point"));
            xmlFile.WriteAttr(wxT("f"), mCurves[ curve ].points[ point ].Freq, 12);
            xmlFile.WriteAttr(wxT("d"), mCurves[ curve ].points[ point ].dB, 12);
            xmlFile.EndTag(wxT("point"));
        }

        // Terminate curve
        xmlFile.EndTag(wxT("curve"));
    }

    // Terminate our hierarchy
    xmlFile.EndTag(wxT("equalizationeffect"));
}
