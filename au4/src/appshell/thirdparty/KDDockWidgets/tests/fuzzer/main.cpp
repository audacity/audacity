/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

// We don't care about performance related checks in the tests
// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro,range-loop,missing-typeinfo,detaching-member,function-args-by-ref,non-pod-global-static,reserve-candidates,qstring-allocations

#include "Fuzzer.h"
#include "DockRegistry_p.h"
#include "../utils.h"

#include <QCommandLineParser>
#include <QApplication>
#include <QTimer>
#include <QDebug>
#include <QFile>
#include <iostream>

using namespace KDDockWidgets;
using namespace KDDockWidgets::Testing;

int main(int argc, char **argv)
{
    if (!qpaPassedAsArgument(argc, argv)) {
        // Use offscreen by default as it's less annoying, doesn't create visible windows
        qputenv("QT_QPA_PLATFORM", "offscreen");
    }

    QApplication app(argc, argv);

    QCommandLineParser parser;
    parser.setApplicationDescription("Fuzzer Help");
    parser.addPositionalArgument("json", QCoreApplication::translate("main", "json file to load"));

    QCommandLineOption slowDownOption("s", QCoreApplication::translate("main", "Slowdown tests. Adds a 1 second delay between operations"));
    parser.addOption(slowDownOption);

    QCommandLineOption forceDumpJsonOption("f", QCoreApplication::translate("main", "Dump json of the test even if we're already loading a test."));
    parser.addOption(forceDumpJsonOption);

    QCommandLineOption loopOption("l", QCoreApplication::translate("main", "Loops until it crashes"));
    parser.addOption(loopOption);

    QCommandLineOption skipLastOption("a", QCoreApplication::translate("main", "Skips the last test (presumably failing)"));
    parser.addOption(skipLastOption);

    QCommandLineOption noQuitOption("n", QCoreApplication::translate("main", "Don't quit at the end, keep event loop running for debugging"));
    parser.addOption(noQuitOption);

    parser.addHelpOption();
    parser.process(app);

    const bool slowDown = parser.isSet(slowDownOption);
    const bool forceDumpJson = parser.isSet(forceDumpJsonOption);

    const QStringList filesToLoad = parser.positionalArguments();
    const bool dumpToJsonOnFatal = forceDumpJson || filesToLoad.isEmpty();


    Fuzzer::Options options = Fuzzer::Option_None;
    if (parser.isSet(skipLastOption))
        options |= Fuzzer::Option_SkipLast;

    if (parser.isSet(noQuitOption))
        options |= Fuzzer::Option_NoQuit;

    const bool loops = parser.isSet(loopOption);

    Fuzzer fuzzer(dumpToJsonOnFatal, options);
    if (slowDown)
        fuzzer.setDelayBetweenOperations(1000);

    for (const QString &file : filesToLoad) {
        if (!QFile::exists(file)) {
            std::cerr << "\nFile doesn't exist: " << file.toStdString() << "\n";
            return 0;
        }
    }

    QTimer::singleShot(0, &fuzzer, [&app, &fuzzer, filesToLoad, loops, options] {
        if (filesToLoad.isEmpty()) {
            do {
                fuzzer.fuzz({ 1, 10, true });
            } while(loops);
        } else {
            fuzzer.fuzz(filesToLoad);
        }

        if (!(options & Fuzzer::Option_NoQuit)) {
            // if noQuit is true we keep the app running so it can be debugged
            app.quit();
        }
    });

    app.setQuitOnLastWindowClosed(false);
    return app.exec();
}
