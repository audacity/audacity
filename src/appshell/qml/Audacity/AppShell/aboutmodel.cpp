/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "aboutmodel.h"

#include <array>

#include <QApplication>
#include <QClipboard>
#include <QUrl>
#include <QVariantList>
#include <qbytearrayview.h>

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

const std::array<CreditEntry, 14> sTeamMembers = { {
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
} };

const std::array<CreditEntry, 28> sEmeritusTeam = { {
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
} };

const std::array<CreditEntry, 65> sContributors = { {
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
} };

const std::array<CreditEntry, 2> sGraphics = { {
    { "Shinta Carolinasari", "web developer" },
    { "Bayu Rizaldhan Rayes", "graphics" },
} };

const std::array<LibraryEntry, 16> sLibraries = { {
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
} };

const std::array<ThanksEntry, 21> sThanks = { {
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
} };

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

template<typename Array>
QVariantList buildCredits(const Array& entries)
{
    QVariantList list;
    list.reserve(static_cast<qsizetype>(entries.size()));
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

QVariantMap makeSection(const char* title, const char* subtitle, QVariantList credits)
{
    QVariantMap s;
    s["title"]   = muse::qtrc("appshell/about", title);
    s["subtitle"] = muse::qtrc("appshell/about", subtitle);
    s["credits"] = std::move(credits);
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
    return {
        makeSection("Audacity Team Members", buildCredits(sTeamMembers)),
        makeSection("Emeritus", "Distinguished Audacity Team members, not currently active", buildCredits(sEmeritusTeam)),
        makeSection("Contributors", buildCredits(sContributors)),
        makeSection("Website and Graphics", buildCredits(sGraphics)),
        makeSection("Libraries", "Audacity includes code from the following projects:", buildCredits(sLibraries)),
        makeSection("Special Thanks", buildCredits(sThanks)),
    };
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
    static const QString gplText = QStringLiteral(
        R"(<h3 style="text-align: center;">GNU GENERAL PUBLIC LICENSE</h3>)"
        R"(<p style="text-align: center;">Version 3, 29 June 2007</p>)"
        R"(<p>Copyright &copy; 2007 Free Software Foundation, Inc.)"
        R"( &lt;<a href="https://fsf.org/">https://fsf.org/</a>&gt;</p><p>)"
        R"( Everyone is permitted to copy and distribute verbatim copies)"
        R"( of this license document, but changing it is not allowed.</p>)"
        R"(<h4 id="preamble">Preamble</h4>)"
        R"(<p>The GNU General Public License is a free, copyleft license for)"
        R"(software and other kinds of works.</p>)"
        R"(<p>The licenses for most software and other practical works are designed)"
        R"(to take away your freedom to share and change the works.  By contrast,)"
        R"(the GNU General Public License is intended to guarantee your freedom to)"
        R"(share and change all versions of a program--to make sure it remains free)"
        R"(software for all its users.  We, the Free Software Foundation, use the)"
        R"(GNU General Public License for most of our software; it applies also to)"
        R"(any other work released this way by its authors.  You can apply it to)"
        R"(your programs, too.</p>)"
        R"(<p>When we speak of free software, we are referring to freedom, not)"
        R"(price.  Our General Public Licenses are designed to make sure that you)"
        R"(have the freedom to distribute copies of free software (and charge for)"
        R"(them if you wish), that you receive source code or can get it if you)"
        R"(want it, that you can change the software or use pieces of it in new)"
        R"(free programs, and that you know you can do these things.</p>)"
        R"(<p>To protect your rights, we need to prevent others from denying you)"
        R"(these rights or asking you to surrender the rights.  Therefore, you have)"
        R"(certain responsibilities if you distribute copies of the software, or if)"
        R"(you modify it: responsibilities to respect the freedom of others.</p>)"
        R"(<p>For example, if you distribute copies of such a program, whether)"
        R"(gratis or for a fee, you must pass on to the recipients the same)"
        R"(freedoms that you received.  You must make sure that they, too, receive)"
        R"(or can get the source code.  And you must show them these terms so they)"
        R"(know their rights.</p>)"
        R"(<p>Developers that use the GNU GPL protect your rights with two steps:)"
        R"((1) assert copyright on the software, and (2) offer you this License)"
        R"(giving you legal permission to copy, distribute and/or modify it.</p>)"
        R"(<p>For the developers' and authors' protection, the GPL clearly explains)"
        R"(that there is no warranty for this free software.  For both users' and)"
        R"(authors' sake, the GPL requires that modified versions be marked as)"
        R"(changed, so that their problems will not be attributed erroneously to)"
        R"(authors of previous versions.</p>)"
        R"(<p>Some devices are designed to deny users access to install or run)"
        R"(modified versions of the software inside them, although the manufacturer)"
        R"(can do so.  This is fundamentally incompatible with the aim of)"
        R"(protecting users' freedom to change the software.  The systematic)"
        R"(pattern of such abuse occurs in the area of products for individuals to)"
        R"(use, which is precisely where it is most unacceptable.  Therefore, we)"
        R"(have designed this version of the GPL to prohibit the practice for those)"
        R"(products.  If such problems arise substantially in other domains, we)"
        R"(stand ready to extend this provision to those domains in future versions)"
        R"(of the GPL, as needed to protect the freedom of users.</p>)"
        R"(<p>Finally, every program is threatened constantly by software patents.)"
        R"(States should not allow patents to restrict development and use of)"
        R"(software on general-purpose computers, but in those that do, we wish to)"
        R"(avoid the special danger that patents applied to a free program could)"
        R"(make it effectively proprietary.  To prevent this, the GPL assures that)"
        R"(patents cannot be used to render the program non-free.</p>)"
        R"(<p>The precise terms and conditions for copying, distribution and)"
        R"(modification follow.</p>)"
        R"(<h4 id="terms">TERMS AND CONDITIONS</h4>)"
        R"(<h5 id="section0">0. Definitions.</h5>)"
        R"(<p>&ldquo;This License&rdquo; refers to version 3 of the GNU General Public License.</p>)"
        R"(<p>&ldquo;Copyright&rdquo; also means copyright-like laws that apply to other kinds of)"
        R"(works, such as semiconductor masks.</p>)"
        R"( )"
        R"(<p>&ldquo;The Program&rdquo; refers to any copyrightable work licensed under this)"
        R"(License.  Each licensee is addressed as &ldquo;you&rdquo;.  &ldquo;Licensees&rdquo; and)"
        R"(&ldquo;recipients&rdquo; may be individuals or organizations.</p>)"
        R"(<p>To &ldquo;modify&rdquo; a work means to copy from or adapt all or part of the work)"
        R"(in a fashion requiring copyright permission, other than the making of an)"
        R"(exact copy.  The resulting work is called a &ldquo;modified version&rdquo; of the)"
        R"(earlier work or a work &ldquo;based on&rdquo; the earlier work.</p>)"
        R"(<p>A &ldquo;covered work&rdquo; means either the unmodified Program or a work based)"
        R"(on the Program.</p>)"
        R"(<p>To &ldquo;propagate&rdquo; a work means to do anything with it that, without)"
        R"(permission, would make you directly or secondarily liable for)"
        R"(infringement under applicable copyright law, except executing it on a)"
        R"(computer or modifying a private copy.  Propagation includes copying,)"
        R"(distribution (with or without modification), making available to the)"
        R"(public, and in some countries other activities as well.</p>)"
        R"(<p>To &ldquo;convey&rdquo; a work means any kind of propagation that enables other)"
        R"(parties to make or receive copies.  Mere interaction with a user through)"
        R"(a computer network, with no transfer of a copy, is not conveying.</p>)"
        R"(<p>An interactive user interface displays &ldquo;Appropriate Legal Notices&rdquo;)"
        R"(to the extent that it includes a convenient and prominently visible)"
        R"(feature that (1) displays an appropriate copyright notice, and (2))"
        R"(tells the user that there is no warranty for the work (except to the)"
        R"(extent that warranties are provided), that licensees may convey the)"
        R"(work under this License, and how to view a copy of this License.  If)"
        R"(the interface presents a list of user commands or options, such as a)"
        R"(menu, a prominent item in the list meets this criterion.</p>)"
        R"(<h5 id="section1">1. Source Code.</h5>)"
        R"(<p>The &ldquo;source code&rdquo; for a work means the preferred form of the work)"
        R"(for making modifications to it.  &ldquo;Object code&rdquo; means any non-source)"
        R"(form of a work.</p>)"
        R"(<p>A &ldquo;Standard Interface&rdquo; means an interface that either is an official)"
        R"(standard defined by a recognized standards body, or, in the case of)"
        R"(interfaces specified for a particular programming language, one that)"
        R"(is widely used among developers working in that language.</p>)"
        R"(<p>The &ldquo;System Libraries&rdquo; of an executable work include anything, other)"
        R"(than the work as a whole, that (a) is included in the normal form of)"
        R"(packaging a Major Component, but which is not part of that Major)"
        R"(Component, and (b) serves only to enable use of the work with that)"
        R"(Major Component, or to implement a Standard Interface for which an)"
        R"(implementation is available to the public in source code form.  A)"
        R"(&ldquo;Major Component&rdquo;, in this context, means a major essential component)"
        R"((kernel, window system, and so on) of the specific operating system)"
        R"((if any) on which the executable work runs, or a compiler used to)"
        R"(produce the work, or an object code interpreter used to run it.</p>)"
        R"(<p>The &ldquo;Corresponding Source&rdquo; for a work in object code form means all)"
        R"(the source code needed to generate, install, and (for an executable)"
        R"(work) run the object code and to modify the work, including scripts to)"
        R"(control those activities.  However, it does not include the work's)"
        R"(System Libraries, or general-purpose tools or generally available free)"
        R"(programs which are used unmodified in performing those activities but)"
        R"(which are not part of the work.  For example, Corresponding Source)"
        R"(includes interface definition files associated with source files for)"
        R"(the work, and the source code for shared libraries and dynamically)"
        R"(linked subprograms that the work is specifically designed to require,)"
        R"(such as by intimate data communication or control flow between those)"
        R"(subprograms and other parts of the work.</p>)"
        R"(<p>The Corresponding Source need not include anything that users)"
        R"(can regenerate automatically from other parts of the Corresponding)"
        R"(Source.</p>)"
        R"(<p>The Corresponding Source for a work in source code form is that)"
        R"(same work.</p>)"
        R"(<h5 id="section2">2. Basic Permissions.</h5>)"
        R"(<p>All rights granted under this License are granted for the term of)"
        R"(copyright on the Program, and are irrevocable provided the stated)"
        R"(conditions are met.  This License explicitly affirms your unlimited)"
        R"(permission to run the unmodified Program.  The output from running a)"
        R"(covered work is covered by this License only if the output, given its)"
        R"(content, constitutes a covered work.  This License acknowledges your)"
        R"(rights of fair use or other equivalent, as provided by copyright law.</p>)"
        R"(<p>You may make, run and propagate covered works that you do not)"
        R"(convey, without conditions so long as your license otherwise remains)"
        R"(in force.  You may convey covered works to others for the sole purpose)"
        R"(of having them make modifications exclusively for you, or provide you)"
        R"(with facilities for running those works, provided that you comply with)"
        R"(the terms of this License in conveying all material for which you do)"
        R"(not control copyright.  Those thus making or running the covered works)"
        R"(for you must do so exclusively on your behalf, under your direction)"
        R"(and control, on terms that prohibit them from making any copies of)"
        R"(your copyrighted material outside their relationship with you.</p>)"
        R"(<p>Conveying under any other circumstances is permitted solely under)"
        R"(the conditions stated below.  Sublicensing is not allowed; section 10)"
        R"(makes it unnecessary.</p>)"
        R"(<h5 id="section3">3. Protecting Users' Legal Rights From Anti-Circumvention Law.</h5>)"
        R"(<p>No covered work shall be deemed part of an effective technological)"
        R"(measure under any applicable law fulfilling obligations under article)"
        R"(11 of the WIPO copyright treaty adopted on 20 December 1996, or)"
        R"(similar laws prohibiting or restricting circumvention of such)"
        R"(measures.</p>)"
        R"(<p>When you convey a covered work, you waive any legal power to forbid)"
        R"(circumvention of technological measures to the extent such circumvention)"
        R"(is effected by exercising rights under this License with respect to)"
        R"(the covered work, and you disclaim any intention to limit operation or)"
        R"(modification of the work as a means of enforcing, against the work's)"
        R"(users, your or third parties' legal rights to forbid circumvention of)"
        R"(technological measures.</p>)"
        R"(<h5 id="section4">4. Conveying Verbatim Copies.</h5>)"
        R"(<p>You may convey verbatim copies of the Program's source code as you)"
        R"(receive it, in any medium, provided that you conspicuously and)"
        R"(appropriately publish on each copy an appropriate copyright notice;)"
        R"(keep intact all notices stating that this License and any)"
        R"(non-permissive terms added in accord with section 7 apply to the code;)"
        R"(keep intact all notices of the absence of any warranty; and give all)"
        R"(recipients a copy of this License along with the Program.</p>)"
        R"(<p>You may charge any price or no price for each copy that you convey,)"
        R"(and you may offer support or warranty protection for a fee.</p>)"
        R"(<h5 id="section5">5. Conveying Modified Source Versions.</h5>)"
        R"(<p>You may convey a work based on the Program, or the modifications to)"
        R"(produce it from the Program, in the form of source code under the)"
        R"(terms of section 4, provided that you also meet all of these conditions:</p>)"
        R"(<p>)"
        R"(<p>a) The work must carry prominent notices stating that you modified)"
        R"(    it, and giving a relevant date.</p>)"
        R"(<p>b) The work must carry prominent notices stating that it is)"
        R"(    released under this License and any conditions added under section)"
        R"(    7.  This requirement modifies the requirement in section 4 to)"
        R"(    &ldquo;keep intact all notices&rdquo;.</p>)"
        R"(<p>c) You must license the entire work, as a whole, under this)"
        R"(    License to anyone who comes into possession of a copy.  This)"
        R"(    License will therefore apply, along with any applicable section 7)"
        R"(    additional terms, to the whole of the work, and all its parts,)"
        R"(    regardless of how they are packaged.  This License gives no)"
        R"(    permission to license the work in any other way, but it does not)"
        R"(    invalidate such permission if you have separately received it.</p>)"
        R"(<p>d) If the work has interactive user interfaces, each must display)"
        R"(    Appropriate Legal Notices; however, if the Program has interactive)"
        R"(    interfaces that do not display Appropriate Legal Notices, your)"
        R"(    work need not make them do so.</p>)"
        R"(</p>)"
        R"(<p>A compilation of a covered work with other separate and independent)"
        R"(works, which are not by their nature extensions of the covered work,)"
        R"(and which are not combined with it such as to form a larger program,)"
        R"(in or on a volume of a storage or distribution medium, is called an)"
        R"(&ldquo;aggregate&rdquo; if the compilation and its resulting copyright are not)"
        R"(used to limit the access or legal rights of the compilation's users)"
        R"(beyond what the individual works permit.  Inclusion of a covered work)"
        R"(in an aggregate does not cause this License to apply to the other)"
        R"(parts of the aggregate.</p>)"
        R"(<h5 id="section6">6. Conveying Non-Source Forms.</h5>)"
        R"(<p>You may convey a covered work in object code form under the terms)"
        R"(of sections 4 and 5, provided that you also convey the)"
        R"(machine-readable Corresponding Source under the terms of this License,)"
        R"(in one of these ways:</p>)"
        R"(<p>)"
        R"(<p>a) Convey the object code in, or embodied in, a physical product)"
        R"(    (including a physical distribution medium), accompanied by the)"
        R"(    Corresponding Source fixed on a durable physical medium)"
        R"(    customarily used for software interchange.</p>)"
        R"(<p>b) Convey the object code in, or embodied in, a physical product)"
        R"(    (including a physical distribution medium), accompanied by a)"
        R"(    written offer, valid for at least three years and valid for as)"
        R"(    long as you offer spare parts or customer support for that product)"
        R"(    model, to give anyone who possesses the object code either (1) a)"
        R"(    copy of the Corresponding Source for all the software in the)"
        R"(    product that is covered by this License, on a durable physical)"
        R"(    medium customarily used for software interchange, for a price no)"
        R"(    more than your reasonable cost of physically performing this)"
        R"(    conveying of source, or (2) access to copy the)"
        R"(    Corresponding Source from a network server at no charge.</p>)"
        R"(<p>c) Convey individual copies of the object code with a copy of the)"
        R"(    written offer to provide the Corresponding Source.  This)"
        R"(    alternative is allowed only occasionally and noncommercially, and)"
        R"(    only if you received the object code with such an offer, in accord)"
        R"(    with subsection 6b.</p>)"
        R"(<p>d) Convey the object code by offering access from a designated)"
        R"(    place (gratis or for a charge), and offer equivalent access to the)"
        R"(    Corresponding Source in the same way through the same place at no)"
        R"(    further charge.  You need not require recipients to copy the)"
        R"(    Corresponding Source along with the object code.  If the place to)"
        R"(    copy the object code is a network server, the Corresponding Source)"
        R"(    may be on a different server (operated by you or a third party))"
        R"(    that supports equivalent copying facilities, provided you maintain)"
        R"(    clear directions next to the object code saying where to find the)"
        R"(    Corresponding Source.  Regardless of what server hosts the)"
        R"(    Corresponding Source, you remain obligated to ensure that it is)"
        R"(    available for as long as needed to satisfy these requirements.</p>)"
        R"(<p>e) Convey the object code using peer-to-peer transmission, provided)"
        R"(    you inform other peers where the object code and Corresponding)"
        R"(    Source of the work are being offered to the general public at no)"
        R"(    charge under subsection 6d.</p>)"
        R"(</p>)"
        R"(<p>A separable portion of the object code, whose source code is excluded)"
        R"(from the Corresponding Source as a System Library, need not be)"
        R"(included in conveying the object code work.</p>)"
        R"(<p>A &ldquo;User Product&rdquo; is either (1) a &ldquo;consumer product&rdquo;, which means any)"
        R"(tangible personal property which is normally used for personal, family,)"
        R"(or household purposes, or (2) anything designed or sold for incorporation)"
        R"(into a dwelling.  In determining whether a product is a consumer product,)"
        R"(doubtful cases shall be resolved in favor of coverage.  For a particular)"
        R"(product received by a particular user, &ldquo;normally used&rdquo; refers to a)"
        R"(typical or common use of that class of product, regardless of the status)"
        R"(of the particular user or of the way in which the particular user)"
        R"(actually uses, or expects or is expected to use, the product.  A product)"
        R"(is a consumer product regardless of whether the product has substantial)"
        R"(commercial, industrial or non-consumer uses, unless such uses represent)"
        R"(the only significant mode of use of the product.</p>)"
        R"(<p>&ldquo;Installation Information&rdquo; for a User Product means any methods,)"
        R"(procedures, authorization keys, or other information required to install)"
        R"(and execute modified versions of a covered work in that User Product from)"
        R"(a modified version of its Corresponding Source.  The information must)"
        R"(suffice to ensure that the continued functioning of the modified object)"
        R"(code is in no case prevented or interfered with solely because)"
        R"(modification has been made.</p>)"
        R"(<p>If you convey an object code work under this section in, or with, or)"
        R"(specifically for use in, a User Product, and the conveying occurs as)"
        R"(part of a transaction in which the right of possession and use of the)"
        R"(User Product is transferred to the recipient in perpetuity or for a)"
        R"(fixed term (regardless of how the transaction is characterized), the)"
        R"(Corresponding Source conveyed under this section must be accompanied)"
        R"(by the Installation Information.  But this requirement does not apply)"
        R"(if neither you nor any third party retains the ability to install)"
        R"(modified object code on the User Product (for example, the work has)"
        R"(been installed in ROM).</p>)"
        R"(<p>The requirement to provide Installation Information does not include a)"
        R"(requirement to continue to provide support service, warranty, or updates)"
        R"(for a work that has been modified or installed by the recipient, or for)"
        R"(the User Product in which it has been modified or installed.  Access to a)"
        R"(network may be denied when the modification itself materially and)"
        R"(adversely affects the operation of the network or violates the rules and)"
        R"(protocols for communication across the network.</p>)"
        R"(<p>Corresponding Source conveyed, and Installation Information provided,)"
        R"(in accord with this section must be in a format that is publicly)"
        R"(documented (and with an implementation available to the public in)"
        R"(source code form), and must require no special password or key for)"
        R"(unpacking, reading or copying.</p>)"
        R"(<h5 id="section7">7. Additional Terms.</h5>)"
        R"(<p>&ldquo;Additional permissions&rdquo; are terms that supplement the terms of this)"
        R"(License by making exceptions from one or more of its conditions.)"
        R"(Additional permissions that are applicable to the entire Program shall)"
        R"(be treated as though they were included in this License, to the extent)"
        R"(that they are valid under applicable law.  If additional permissions)"
        R"(apply only to part of the Program, that part may be used separately)"
        R"(under those permissions, but the entire Program remains governed by)"
        R"(this License without regard to the additional permissions.</p>)"
        R"(<p>When you convey a copy of a covered work, you may at your option)"
        R"(remove any additional permissions from that copy, or from any part of)"
        R"(it.  (Additional permissions may be written to require their own)"
        R"(removal in certain cases when you modify the work.)  You may place)"
        R"(additional permissions on material, added by you to a covered work,)"
        R"(for which you have or can give appropriate copyright permission.</p>)"
        R"(<p>Notwithstanding any other provision of this License, for material you)"
        R"(add to a covered work, you may (if authorized by the copyright holders of)"
        R"(that material) supplement the terms of this License with terms:</p>)"
        R"(<p>)"
        R"(<p>a) Disclaiming warranty or limiting liability differently from the)"
        R"(    terms of sections 15 and 16 of this License; or</p>)"
        R"(<p>b) Requiring preservation of specified reasonable legal notices or)"
        R"(    author attributions in that material or in the Appropriate Legal)"
        R"(    Notices displayed by works containing it; or</p>)"
        R"(<p>c) Prohibiting misrepresentation of the origin of that material, or)"
        R"(    requiring that modified versions of such material be marked in)"
        R"(    reasonable ways as different from the original version; or</p>)"
        R"(<p>d) Limiting the use for publicity purposes of names of licensors or)"
        R"(    authors of the material; or</p>)"
        R"(<p>e) Declining to grant rights under trademark law for use of some)"
        R"(    trade names, trademarks, or service marks; or</p>)"
        R"(<p>f) Requiring indemnification of licensors and authors of that)"
        R"(    material by anyone who conveys the material (or modified versions of)"
        R"(    it) with contractual assumptions of liability to the recipient, for)"
        R"(    any liability that these contractual assumptions directly impose on)"
        R"(    those licensors and authors.</p>)"
        R"(</p>)"
        R"(<p>All other non-permissive additional terms are considered &ldquo;further)"
        R"(restrictions&rdquo; within the meaning of section 10.  If the Program as you)"
        R"(received it, or any part of it, contains a notice stating that it is)"
        R"(governed by this License along with a term that is a further)"
        R"(restriction, you may remove that term.  If a license document contains)"
        R"(a further restriction but permits relicensing or conveying under this)"
        R"(License, you may add to a covered work material governed by the terms)"
        R"(of that license document, provided that the further restriction does)"
        R"(not survive such relicensing or conveying.</p>)"
        R"(<p>If you add terms to a covered work in accord with this section, you)"
        R"(must place, in the relevant source files, a statement of the)"
        R"(additional terms that apply to those files, or a notice indicating)"
        R"(where to find the applicable terms.</p>)"
        R"(<p>Additional terms, permissive or non-permissive, may be stated in the)"
        R"(form of a separately written license, or stated as exceptions;)"
        R"(the above requirements apply either way.</p>)"
        R"(<h5 id="section8">8. Termination.</h5>)"
        R"(<p>You may not propagate or modify a covered work except as expressly)"
        R"(provided under this License.  Any attempt otherwise to propagate or)"
        R"(modify it is void, and will automatically terminate your rights under)"
        R"(this License (including any patent licenses granted under the third)"
        R"(paragraph of section 11).</p>)"
        R"(<p>However, if you cease all violation of this License, then your)"
        R"(license from a particular copyright holder is reinstated (a))"
        R"(provisionally, unless and until the copyright holder explicitly and)"
        R"(finally terminates your license, and (b) permanently, if the copyright)"
        R"(holder fails to notify you of the violation by some reasonable means)"
        R"(prior to 60 days after the cessation.</p>)"
        R"(<p>Moreover, your license from a particular copyright holder is)"
        R"(reinstated permanently if the copyright holder notifies you of the)"
        R"(violation by some reasonable means, this is the first time you have)"
        R"(received notice of violation of this License (for any work) from that)"
        R"(copyright holder, and you cure the violation prior to 30 days after)"
        R"(your receipt of the notice.</p>)"
        R"(<p>Termination of your rights under this section does not terminate the)"
        R"(licenses of parties who have received copies or rights from you under)"
        R"(this License.  If your rights have been terminated and not permanently)"
        R"(reinstated, you do not qualify to receive new licenses for the same)"
        R"(material under section 10.</p>)"
        R"(<h5 id="section9">9. Acceptance Not Required for Having Copies.</h5>)"
        R"(<p>You are not required to accept this License in order to receive or)"
        R"(run a copy of the Program.  Ancillary propagation of a covered work)"
        R"(occurring solely as a consequence of using peer-to-peer transmission)"
        R"(to receive a copy likewise does not require acceptance.  However,)"
        R"(nothing other than this License grants you permission to propagate or)"
        R"(modify any covered work.  These actions infringe copyright if you do)"
        R"(not accept this License.  Therefore, by modifying or propagating a)"
        R"(covered work, you indicate your acceptance of this License to do so.</p>)"
        R"(<h5 id="section10">10. Automatic Licensing of Downstream Recipients.</h5>)"
        R"(<p>Each time you convey a covered work, the recipient automatically)"
        R"(receives a license from the original licensors, to run, modify and)"
        R"(propagate that work, subject to this License.  You are not responsible)"
        R"(for enforcing compliance by third parties with this License.</p>)"
        R"(<p>An &ldquo;entity transaction&rdquo; is a transaction transferring control of an)"
        R"(organization, or substantially all assets of one, or subdividing an)"
        R"(organization, or merging organizations.  If propagation of a covered)"
        R"(work results from an entity transaction, each party to that)"
        R"(transaction who receives a copy of the work also receives whatever)"
        R"(licenses to the work the party's predecessor in interest had or could)"
        R"(give under the previous paragraph, plus a right to possession of the)"
        R"(Corresponding Source of the work from the predecessor in interest, if)"
        R"(the predecessor has it or can get it with reasonable efforts.</p>)"
        R"(<p>You may not impose any further restrictions on the exercise of the)"
        R"(rights granted or affirmed under this License.  For example, you may)"
        R"(not impose a license fee, royalty, or other charge for exercise of)"
        R"(rights granted under this License, and you may not initiate litigation)"
        R"((including a cross-claim or counterclaim in a lawsuit) alleging that)"
        R"(any patent claim is infringed by making, using, selling, offering for)"
        R"(sale, or importing the Program or any portion of it.</p>)"
        R"(<h5 id="section11">11. Patents.</h5>)"
        R"(<p>A &ldquo;contributor&rdquo; is a copyright holder who authorizes use under this)"
        R"(License of the Program or a work on which the Program is based.  The)"
        R"(work thus licensed is called the contributor's &ldquo;contributor version&rdquo;.</p>)"
        R"(<p>A contributor's &ldquo;essential patent claims&rdquo; are all patent claims)"
        R"(owned or controlled by the contributor, whether already acquired or)"
        R"(hereafter acquired, that would be infringed by some manner, permitted)"
        R"(by this License, of making, using, or selling its contributor version,)"
        R"(but do not include claims that would be infringed only as a)"
        R"(consequence of further modification of the contributor version.  For)"
        R"(purposes of this definition, &ldquo;control&rdquo; includes the right to grant)"
        R"(patent sublicenses in a manner consistent with the requirements of)"
        R"(this License.</p>)"
        R"(<p>Each contributor grants you a non-exclusive, worldwide, royalty-free)"
        R"(patent license under the contributor's essential patent claims, to)"
        R"(make, use, sell, offer for sale, import and otherwise run, modify and)"
        R"(propagate the contents of its contributor version.</p>)"
        R"(<p>In the following three paragraphs, a &ldquo;patent license&rdquo; is any express)"
        R"(agreement or commitment, however denominated, not to enforce a patent)"
        R"((such as an express permission to practice a patent or covenant not to)"
        R"(sue for patent infringement).  To &ldquo;grant&rdquo; such a patent license to a)"
        R"(party means to make such an agreement or commitment not to enforce a)"
        R"(patent against the party.</p>)"
        R"(<p>If you convey a covered work, knowingly relying on a patent license,)"
        R"(and the Corresponding Source of the work is not available for anyone)"
        R"(to copy, free of charge and under the terms of this License, through a)"
        R"(publicly available network server or other readily accessible means,)"
        R"(then you must either (1) cause the Corresponding Source to be so)"
        R"(available, or (2) arrange to deprive yourself of the benefit of the)"
        R"(patent license for this particular work, or (3) arrange, in a manner)"
        R"(consistent with the requirements of this License, to extend the patent)"
        R"(license to downstream recipients.  &ldquo;Knowingly relying&rdquo; means you have)"
        R"(actual knowledge that, but for the patent license, your conveying the)"
        R"(covered work in a country, or your recipient's use of the covered work)"
        R"(in a country, would infringe one or more identifiable patents in that)"
        R"(country that you have reason to believe are valid.</p>)"
        R"(  )"
        R"(<p>If, pursuant to or in connection with a single transaction or)"
        R"(arrangement, you convey, or propagate by procuring conveyance of, a)"
        R"(covered work, and grant a patent license to some of the parties)"
        R"(receiving the covered work authorizing them to use, propagate, modify)"
        R"(or convey a specific copy of the covered work, then the patent license)"
        R"(you grant is automatically extended to all recipients of the covered)"
        R"(work and works based on it.</p>)"
        R"(<p>A patent license is &ldquo;discriminatory&rdquo; if it does not include within)"
        R"(the scope of its coverage, prohibits the exercise of, or is)"
        R"(conditioned on the non-exercise of one or more of the rights that are)"
        R"(specifically granted under this License.  You may not convey a covered)"
        R"(work if you are a party to an arrangement with a third party that is)"
        R"(in the business of distributing software, under which you make payment)"
        R"(to the third party based on the extent of your activity of conveying)"
        R"(the work, and under which the third party grants, to any of the)"
        R"(parties who would receive the covered work from you, a discriminatory)"
        R"(patent license (a) in connection with copies of the covered work)"
        R"(conveyed by you (or copies made from those copies), or (b) primarily)"
        R"(for and in connection with specific products or compilations that)"
        R"(contain the covered work, unless you entered into that arrangement,)"
        R"(or that patent license was granted, prior to 28 March 2007.</p>)"
        R"(<p>Nothing in this License shall be construed as excluding or limiting)"
        R"(any implied license or other defenses to infringement that may)"
        R"(otherwise be available to you under applicable patent law.</p>)"
        R"(<h5 id="section12">12. No Surrender of Others' Freedom.</h5>)"
        R"(<p>If conditions are imposed on you (whether by court order, agreement or)"
        R"(otherwise) that contradict the conditions of this License, they do not)"
        R"(excuse you from the conditions of this License.  If you cannot convey a)"
        R"(covered work so as to satisfy simultaneously your obligations under this)"
        R"(License and any other pertinent obligations, then as a consequence you may)"
        R"(not convey it at all.  For example, if you agree to terms that obligate you)"
        R"(to collect a royalty for further conveying from those to whom you convey)"
        R"(the Program, the only way you could satisfy both those terms and this)"
        R"(License would be to refrain entirely from conveying the Program.</p>)"
        R"(<h5 id="section13">13. Use with the GNU Affero General Public License.</h5>)"
        R"(<p>Notwithstanding any other provision of this License, you have)"
        R"(permission to link or combine any covered work with a work licensed)"
        R"(under version 3 of the GNU Affero General Public License into a single)"
        R"(combined work, and to convey the resulting work.  The terms of this)"
        R"(License will continue to apply to the part which is the covered work,)"
        R"(but the special requirements of the GNU Affero General Public License,)"
        R"(section 13, concerning interaction through a network will apply to the)"
        R"(combination as such.</p>)"
        R"(<h5 id="section14">14. Revised Versions of this License.</h5>)"
        R"(<p>The Free Software Foundation may publish revised and/or new versions of)"
        R"(the GNU General Public License from time to time.  Such new versions will)"
        R"(be similar in spirit to the present version, but may differ in detail to)"
        R"(address new problems or concerns.</p>)"
        R"(<p>Each version is given a distinguishing version number.  If the)"
        R"(Program specifies that a certain numbered version of the GNU General)"
        R"(Public License &ldquo;or any later version&rdquo; applies to it, you have the)"
        R"(option of following the terms and conditions either of that numbered)"
        R"(version or of any later version published by the Free Software)"
        R"(Foundation.  If the Program does not specify a version number of the)"
        R"(GNU General Public License, you may choose any version ever published)"
        R"(by the Free Software Foundation.</p>)"
        R"(<p>If the Program specifies that a proxy can decide which future)"
        R"(versions of the GNU General Public License can be used, that proxy's)"
        R"(public statement of acceptance of a version permanently authorizes you)"
        R"(to choose that version for the Program.</p>)"
        R"(<p>Later license versions may give you additional or different)"
        R"(permissions.  However, no additional obligations are imposed on any)"
        R"(author or copyright holder as a result of your choosing to follow a)"
        R"(later version.</p>)"
        R"(<h5 id="section15">15. Disclaimer of Warranty.</h5>)"
        R"(<p>THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY)"
        R"(APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT)"
        R"(HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM &ldquo;AS IS&rdquo; WITHOUT WARRANTY)"
        R"(OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,)"
        R"(THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR)"
        R"(PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM)"
        R"(IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF)"
        R"(ALL NECESSARY SERVICING, REPAIR OR CORRECTION.</p>)"
        R"(<h5 id="section16">16. Limitation of Liability.</h5>)"
        R"(<p>IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING)"
        R"(WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS)"
        R"(THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY)"
        R"(GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE)"
        R"(USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF)"
        R"(DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD)"
        R"(PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),)"
        R"(EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF)"
        R"(SUCH DAMAGES.</p>)"
        R"(<h5 id="section17">17. Interpretation of Sections 15 and 16.</h5>)"
        R"(<p>If the disclaimer of warranty and limitation of liability provided)"
        R"(above cannot be given local legal effect according to their terms,)"
        R"(reviewing courts shall apply local law that most closely approximates)"
        R"(an absolute waiver of all civil liability in connection with the)"
        R"(Program, unless a warranty or assumption of liability accompanies a)"
        R"(copy of the Program in return for a fee.</p>)");

    return gplText;
}
