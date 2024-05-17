#include "Internat.h"

namespace 
{
    const TranslatableStrings pendingStrings {
        //i6364-whats-new-dialog-update
        XO("Get free plugins & sounds"),
        XO("<p>Check out our [[%s|Muse Hub app]] for a wide range of audio plugins for Audacity users</p>"),
        //i18n-hint: hyperlink
        XO("View tutorials"),
        //i18n-hint: hyperlink
        XO("Visit our forum"),

        //i6156-plugin-locations
        //i18n-hint: Title of the buttons that adds new effects plugin search path 
        XO("Add new location"),
        /*i18n-hint: Title of the panel containing user-defined paths where plugins could be found
        First argument is replaced with plugin type (e.g. "LV2 plugin locations")*/
        XO("%s plugin locations"),

        //master-channel
        //i18n-hint: master channel display name
        XO("Master"),
        //i18n-hint: Title of a section of realtime effects panel where effects that are applied to all tracks could be added 
        XO("Master effects"),
        //i18n-hint: Hint for a master effects section of realtime effects panel
        XO("Apply to all tracks"),

        //realtime-limiter-ui
        XO("Reduces \"dynamic range\", or differences between loud and quiet parts."),
        XO("Compression curve"),
        XO("Smoothing"),
        XO("Input gain (dB)"),
        XO("Ceiling (dB)"),
        XO("Input"),
        XO("Attenuation"),
        XO("&Threshold (dB)"),
        XO("&Make-up gain (dB)"),
        XO("&Make-up target (dB)"),
        XO("Knee &width (dB)"),
        XO("Rati&o:"),
        XO("&Lookahead (ms)"),
        XO("Attac&k (ms)"),
        XO("&Release (ms)"),
        XO("awaiting playback"),
        XO("Augments loudness while minimizing distortion."),

        //
        XO("Limiter (beta)"),
        XO("Compressor (beta)"),
        XO("&Show graph (beta)"),

        //import strings
        //i18n-hint: import progress dialog title, progress dialog
        XO("Importing files"), 
        //i18n-hint: import progress dialot text, where %s is the filename
        XO("Importing %s..."),
        XO("Cancel")
    };
}
