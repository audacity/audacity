/*
* Audacity: A Digital Audio Editor
*/
#include "aboutmodel.h"

#include <QApplication>
#include <QClipboard>
#include <QUrl>
#include <QVariantList>

#include "framework/global/translation.h"

using namespace muse;
using namespace au::appshell;

namespace {
struct CreditEntry {
    const char* name;
    const char* role;
};

struct LibraryEntry {
    const char* name;
    const char* url;
};

struct ThanksEntry {
    const char* name;
};

constexpr CreditEntry sTeamMembers[] =  {
    { "dozzzzer", "quality assurance" },
    { "Igor Korsukov", "developer" },
    { "Jessica Williamson", "designer" },
    { "Paul Martin", "audio developer, @luapmartin" },
    { "Alexander Dawson", "designer" },
    { "Gabriel Sartori", "developer" },
    { "Yana Larina", "project manager" },
    { "Matthieu Hodgkinson", "audio developer" },
    { "Dmitry Makarenko", "developer" },
    { "Elnur Ismailzada", "developer" },
    { "Johan Althoff", "designer" },
    { "Martin Keary", nullptr },
    { "Grzegorz Wojciechowski", "developer" },
    { "Antons \xc4\x8cinakovs", "quality assurance" }
};

constexpr CreditEntry sEmeritusTeam[] = {
    { "Gale Andrews", "quality assurance" },
    { "Richard Ash", "developer" },
    { "Christian Brochec", "documentation and support, French" },
    { "Matt Brubeck", "developer" },
    { "Arturo \"Buanzo\" Busleiman", "system administration" },
    { "Michael Chinen", "developer" },
    { "James Crook", "developer" },
    { "Roger Dannenberg", "co-founder and developer" },
    { "Steve Daulton", nullptr },
    { "Al Dimond", "developer" },
    { "Benjamin Drung", "developer" },
    { "Joshua Haberman", "developer" },
    { "Ruslan Ijbulatov", "developer" },
    { "Vaughan Johnson", "developer" },
    { "Greg Kozikowski", "documentation and support" },
    { "Paul Licameli", "developer" },
    { "Leland Lucius", "developer" },
    { "Dominic Mazzoni", "co-founder and developer" },
    { "Markus Meyer", "developer" },
    { "Monty Montgomery", "developer" },
    { "Shane Mueller", "developer" },
    { "Tony Oetzmann", "documentation and support" },
    { "Alexandre Prokoudine", "documentation and support" },
    { "Peter Sampson", "QA tester, documentation and support" },
    { "Martyn Shaw", "developer" },
    { "Dmitry Vedenko", "developer" },
    { "Bill Wharrie", "documentation and support" },
    { "Casper Jeukendrup", "developer" }
};

constexpr CreditEntry sContributors[] = {
    { "Leo Wattenberg", "designer" },
    { "Peter Jonas", "developer" },
    { "Lynn Allan", "developer" },
    { "Brian Armstrong", "developer" },
    { "David Avery", "developer" },
    { "David Bailes", "accessibility advisor" },
    { "Brian Beard (Kurtsley)", "developer" },
    { "William Bland", "developer" },
    { "Sami Boukortt", "developer" },
    { "Jeremy R. Brown", "developer" },
    { "Alex S. Brown", "developer" },
    { "David Bryant", "developer" },
    { "Chris Cannam", "developer" },
    { "Subhradeep Chakraborty", "developer" },
    { "Cory Cook", "developer" },
    { "Craig DeForest", "developer" },
    { "Edgar Franke (Edgar-RFT)", "developer" },
    { "Anton Gerasimov", "developer" },
    { "Mitch Golden", "developer" },
    { "Brian Gunlogson", "developer" },
    { "Gonzalo Guzm\xc3\xa1n", "documentation and support" },
    { "Andrew Hallendorff", "developer" },
    { "Robert H\xc3\xa4nggi", "developer" },
    { "Jouni Helminen", "designer" },
    { "Daniel Horgan", "developer" },
    { "David Hostetler", "developer" },
    { "Edward Hui", "developer" },
    { "Vladislav Isaev", "effects presets" },
    { "Marek Iwaszkiewicz", "effects presets" },
    { "Steve Jolly", "developer" },
    { "Steven Jones", "developer" },
    { "Henric Jungheim", "developer" },
    { "Myungchul Keum", "developer" },
    { "Arun Kishore", "developer" },
    { "Paul Livesey", "developer" },
    { "Harvey Lubin", "graphic artist" },
    { "Max Maisel", "developer" },
    { "Pietro Marcello", "developer" },
    { "Greg Mekkes", "developer" },
    { "Abe Milde", "developer" },
    { "Ryan Miller", "tester" },
    { "Paul Nasca", "developer" },
    { "Clayton Otey", "developer" },
    { "Pavel Penikov", "tester" },
    { "Mark Phillips", "developer" },
    { "Andr\xc3\xa9 Pinto", "developer" },
    { "Pokechu22", "developer" },
    { "Jean Claude Risset", "composer" },
    { "RuRo", "developer" },
    { "Augustus Saunders", "developer" },
    { "Benjamin Schwartz", "developer" },
    { "Cliff Scott", "tester" },
    { "David R. Sky", "Nyquist plug-ins" },
    { "Joe Souza", "developer" },
    { "K. Soze", "developer" },
    { "Rob Sykes", "developer" },
    { "Mike Underwood", "developer" },
    { "Philip Van Baren", "developer" },
    { "Salvo Ventura", "developer" },
    { "Darrell Walisser", "developer" },
    { "Jun Wan", "developer" },
    { "Daniel Winzen", "developer" },
    { "Tom Woodhams", "developer" },
    { "Mark Young", "developer" },
    { "Wing Yu", "developer" },
};

constexpr CreditEntry sGraphics[] = {
    { "Shinta Carolinasari", "web developer" },
    { "Bayu Rizaldhan Rayes", "graphics" },
};

constexpr LibraryEntry sLibraries[] = {
    { "expat", "https://libexpat.github.io/" },
    { "FLAC", "https://xiph.org/flac/" },
    { "LAME", "http://lame.sourceforge.net/" },
    { "libsndfile", "http://www.mega-nerd.com/libsndfile/" },
    { "libsoxr", "https://sourceforge.net/p/soxr/wiki/Home/" },
    { "lv2", "http://lv2plug.in/" },
    { "Nyquist", "https://www.cs.cmu.edu/~music/nyquist/" },
    { "Ogg Vorbis", "https://xiph.org/vorbis/" },
    { "PortAudio", "http://www.portaudio.com/" },
    { "PortMidi", "http://www.portmedia.sourceforge.net/portmidi/" },
    { "portsmf", "https://sourceforge.net/p/portmedia/wiki/portsmf/" },
    { "sbsms", "http://sbsms.sourceforge.net/" },
    { "SoundTouch", "https://www.surina.net/soundtouch/" },
    { "TwoLAME", "http://www.twolame.org/" },
    { "Vamp", "http://www.vamp-plugins.org/" },
    { "wxWidgets", "https://wxwidgets.org/" },
};

constexpr ThanksEntry sThanks[] = {
    { "Dave Beydler" },
    { "Brian Cameron" },
    { "Jason Cohen" },
    { "Dave Fancella" },
    { "Steve Harris" },
    { "Daniel James" },
    { "Daniil Kolpakov" },
    { "Robert Leidle" },
    { "Logan Lewis" },
    { "David Luff" },
    { "Jason Pepas" },
    { "Jonathan Ryshpan" },
    { "Michael Schwendt" },
    { "Patrick Shirkey" },
    { "Tuomas Suutari" },
    { "Mark Tomlinson" },
    { "David Topper" },
    { "Rudy Trubitt" },
    { "StreetIQ.com" },
    { "UmixIt Technologies, LLC" },
    { "Verilogix, Inc." },
};

QVariantMap makeCredit(const CreditEntry& e)
{
    QVariantMap m;
    m["name"] = QString::fromUtf8(e.name);
    m["role"] = e.role ? QString::fromUtf8(e.role) : QString();
    m["url"]  = QString();
    return m;
}

QVariantMap makeCredit(const LibraryEntry& e)
{
    QVariantMap m;
    m["name"] = QString::fromUtf8(e.name);
    m["role"] = QString();
    m["url"]  = QString::fromUtf8(e.url);
    return m;
}

QVariantMap makeCredit(const ThanksEntry& e)
{
    QVariantMap m;
    m["name"] = QString::fromUtf8(e.name);
    m["role"] = QString();
    m["url"]  = QString();
    return m;
}

template<typename ArrayTypes>
QVariantList buildCredits(const ArrayTypes& entries)
{
    QVariantList list;
    for (const auto& e : entries) {
        list.append(makeCredit(e));
    }
    return list;
}

QVariantMap makeSection(const char* title, const QVariantList& credits)
{
    QVariantMap s;
    s["title"]   = muse::qtrc("appshell/about", title);
    s["credits"] = credits;
    return s;
}

QVariantMap makeSection(const char* title, const char* subtitle, const QVariantList& credits)
{
    QVariantMap s;
    s["title"]   = muse::qtrc("appshell/about", title);
    s["subtitle"] = muse::qtrc("appshell/about", subtitle);
    s["credits"] = credits;
    return s;
}

QVariantMap makeSection(const char* title, QString rawCredit)
{
    QVariantMap rawCreditMap;
    rawCreditMap["raw"] = rawCredit;

    QVariantMap s;
    s["title"]   = muse::qtrc("appshell/about", title);
    s["credits"] = QVariantList{
        rawCreditMap
    };
    return s;
}
}

AboutModel::AboutModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

QString AboutModel::appVersion() const
{
    QString version = QString::fromStdString(configuration()->audacityVersion());
    return application()->unstable()
           ? muse::qtrc("appshell/about", "Unstable prerelease for %1").arg(version)
           : version;
}

QString AboutModel::appRevision() const
{
    return QString::fromStdString(configuration()->appRevision());
}

QVariantMap AboutModel::appUrl() const
{
    QUrl url(QString::fromStdString(configuration()->appUrl()));
    return makeUrl(url, false);
}

QVariantMap AboutModel::forumUrl() const
{
    QUrl url(QString::fromStdString(configuration()->forumUrl()));
    return makeUrl(url);
}

QVariantMap AboutModel::contributionUrl() const
{
    QUrl url(QString::fromStdString(configuration()->contributionUrl()));
    return makeUrl(url);
}

QVariantMap AboutModel::privacyPolicyUrl() const
{
    QUrl url(QString::fromStdString(updateConfiguration()->privacyPolicyUrl()));
    return makeUrl(url);
}

void AboutModel::copyRevisionToClipboard() const
{
    QApplication::clipboard()->setText(
        QString("OS: %1, Arch.: %2, Audacity version (%3-bit): %4-%5, revision: github-audacity-audacity-%6")
        .arg(QSysInfo::prettyProductName()
             + ((QSysInfo::productType() == "windows" && (QSysInfo::productVersion() == "10" || QSysInfo::productVersion() == "11"))
                ? " or later" : ""))
        .arg(QSysInfo::currentCpuArchitecture())
        .arg(QSysInfo::WordSize)
        .arg(application()->version().toString())
        .arg(application()->build())
        .arg(application()->revision()));
}

void AboutModel::toggleDevMode()
{
    globalConfiguration()->setDevModeEnabled(!globalConfiguration()->devModeEnabled());
}

QVariantList AboutModel::creditList() const
{
    static const QVariantList cached = [] {
        QVariantList creditList = {
            makeSection("Audacity Team Members", buildCredits(sTeamMembers)),
            makeSection("Emeritus", "Distinguished Audacity Team members, not currently active", buildCredits(sEmeritusTeam)),
            makeSection("Contributors", buildCredits(sContributors)),
            makeSection("Website and Graphics", buildCredits(sGraphics)),
        };

        /* i18n-hint: Replace this with the names of the translators for your language.
            This will appear in the Translators section of the About dialog.
            For example: "French translation by Jane Doe<br>John Smith"
            Leave untranslated to hide the Translators section. */
        QString translationCredits = muse::qtrc("appshell/about", "translator_credits");
        if (translationCredits != "translator_credits") {
            creditList.append(makeSection("Translators", translationCredits));
        }

        creditList.append(makeSection("Libraries", "Audacity includes code from the following projects:", buildCredits(sLibraries)));
        creditList.append(makeSection("Special Thanks", buildCredits(sThanks)));
        return creditList;
    }();
    return cached;
}

QVariantMap AboutModel::makeUrl(const QUrl& url, bool showPath) const
{
    QVariantMap urlMap;
    urlMap["url"] = url.toString();
    urlMap["displayName"] = showPath ? url.host() + url.path() : url.host();

    return urlMap;
}

QString AboutModel::gplText() const
{
    static const QString gplText = [this] {
        auto result = fileSystem()->readFile(":/resources/gpl-30.html");
        if (!result.ret) {
            return QString{};
        }
        return QString::fromUtf8(result.val.constChar(), static_cast<qsizetype>(result.val.size()));
    }();

    return gplText;
}
