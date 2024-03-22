/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Class to save and restore dock widget layouts.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "LayoutSaver.h"
#include "Config.h"
#include "MainWindowBase.h"
#include "DockWidgetBase.h"
#include "FrameworkWidgetFactory.h"

#include "private/LayoutSaver_p.h"
#include "private/DockRegistry_p.h"
#include "private/DockWidgetBase_p.h"
#include "private/FloatingWindow_p.h"
#include "private/Frame_p.h"
#include "private/LayoutWidget_p.h"
#include "private/Logging_p.h"
#include "private/Position_p.h"
#include "private/Utils_p.h"

#include <qmath.h>
#include <QDebug>
#include <QFile>

/**
 * Some implementation details:
 *
 * Restoring is done in two phases. From the JSON, we construct an intermediate representation,
 * which doesn't have any GUI types. Finally we then construct the GUI from the intermediate
 * representation.
 *
 *     JSON <-> Intermediate rep (bunch non-gui structs) <-> GUI classes
 *
 * This is in contrast to many other dock widget frameworks which just do:
 *     serialized <-> GUI
 *
 * The advantage of having the intermediate structs is that we can do validations on them and if
 * we find some corruption we don't even start messing with the GUI.
 *
 * See the LayoutSaver::* structs in LayoutSaver_p.h, those are the intermediate structs.
 * They have methods to convert to/from JSON.
 * All other gui classes have methods to convert to/from these structs. For example
 * FloatingWindow::serialize()/deserialize()
 */
using namespace KDDockWidgets;

QHash<QString, LayoutSaver::DockWidget::Ptr> LayoutSaver::DockWidget::s_dockWidgets;
LayoutSaver::Layout *LayoutSaver::Layout::s_currentLayoutBeingRestored = nullptr;


inline InternalRestoreOptions internalRestoreOptions(RestoreOptions options)
{
    if (options == RestoreOption_None) {
        return InternalRestoreOption::None;
    } else if (options == RestoreOption_RelativeToMainWindow) {
        return InternalRestoreOptions(InternalRestoreOption::SkipMainWindowGeometry)
            | InternalRestoreOption::RelativeFloatingWindowGeometry;
    } else {
        qWarning() << Q_FUNC_INFO << "Unknown options" << options;
        return {};
    }
}

bool LayoutSaver::Private::s_restoreInProgress = false;

static QVariantList stringListToVariant(const QStringList &strs)
{
    QVariantList variantList;
    variantList.reserve(strs.size());
    for (const QString &str : strs)
        variantList.push_back(str);

    return variantList;
}

static QStringList variantToStringList(const QVariantList &variantList)
{
    QStringList stringList;
    stringList.reserve(variantList.size());
    for (const QVariant &variant : variantList)
        stringList.push_back(variant.toString());

    return stringList;
}

LayoutSaver::LayoutSaver(RestoreOptions options)
    : d(new Private(options))
{
}

LayoutSaver::~LayoutSaver()
{
    delete d;
}

bool LayoutSaver::saveToFile(const QString &jsonFilename)
{
    const QByteArray data = serializeLayout();

    QFile f(jsonFilename);
    if (!f.open(QIODevice::WriteOnly)) {
        qWarning() << Q_FUNC_INFO << "Failed to open" << jsonFilename << f.errorString();
        return false;
    }

    f.write(data);
    return true;
}

bool LayoutSaver::restoreFromFile(const QString &jsonFilename)
{
    QFile f(jsonFilename);
    if (!f.open(QIODevice::ReadOnly)) {
        qWarning() << Q_FUNC_INFO << "Failed to open" << jsonFilename << f.errorString();
        return false;
    }

    const QByteArray data = f.readAll();
    const bool result = restoreLayout(data);

    return result;
}

QByteArray LayoutSaver::serializeLayout() const
{
    if (!d->m_dockRegistry->isSane()) {
        qWarning() << Q_FUNC_INFO << "Refusing to serialize this layout. Check previous warnings.";
        return {};
    }

    LayoutSaver::Layout layout;

    // Just a simplification. One less type of windows to handle.
    d->m_dockRegistry->ensureAllFloatingWidgetsAreMorphed();

    const MainWindowBase::List mainWindows = d->m_dockRegistry->mainwindows();
    layout.mainWindows.reserve(mainWindows.size());
    for (MainWindowBase *mainWindow : mainWindows) {
        if (d->matchesAffinity(mainWindow->affinities()))
            layout.mainWindows.push_back(mainWindow->serialize());
    }

    const QVector<KDDockWidgets::FloatingWindow *> floatingWindows = d->m_dockRegistry->floatingWindows();
    layout.floatingWindows.reserve(floatingWindows.size());
    for (KDDockWidgets::FloatingWindow *floatingWindow : floatingWindows) {
        if (d->matchesAffinity(floatingWindow->affinities()))
            layout.floatingWindows.push_back(floatingWindow->serialize());
    }

    // Closed dock widgets also have interesting things to save, like geometry and placeholder info
    const DockWidgetBase::List closedDockWidgets = d->m_dockRegistry->closedDockwidgets();
    layout.closedDockWidgets.reserve(closedDockWidgets.size());
    for (DockWidgetBase *dockWidget : closedDockWidgets) {
        if (d->matchesAffinity(dockWidget->affinities()))
            layout.closedDockWidgets.push_back(dockWidget->d->serialize());
    }

    // Save the placeholder info. We do it last, as we also restore it last, since we need all items to be created
    // before restoring the placeholders

    const DockWidgetBase::List dockWidgets = d->m_dockRegistry->dockwidgets();
    layout.allDockWidgets.reserve(dockWidgets.size());
    for (DockWidgetBase *dockWidget : dockWidgets) {
        if (d->matchesAffinity(dockWidget->affinities())) {
            auto dw = dockWidget->d->serialize();
            dw->lastPosition = dockWidget->d->lastPositions().serialize();
            layout.allDockWidgets.push_back(dw);
        }
    }

    return layout.toJson();
}

bool LayoutSaver::restoreLayout(const QByteArray &data)
{
    d->clearRestoredProperty();
    if (data.isEmpty())
        return true;

    struct FrameCleanup
    {
        FrameCleanup(LayoutSaver *saver)
            : m_saver(saver)
        {
        }

        ~FrameCleanup()
        {
            m_saver->d->deleteEmptyFrames();
        }

        LayoutSaver *const m_saver;
    };

    FrameCleanup cleanup(this);
    LayoutSaver::Layout layout;
    if (!layout.fromJson(data)) {
        qWarning() << Q_FUNC_INFO << "Failed to parse json data";
        return false;
    }

    if (!layout.isValid()) {
        return false;
    }

    layout.scaleSizes(d->m_restoreOptions);

    d->floatWidgetsWhichSkipRestore(layout.mainWindowNames());
    d->floatUnknownWidgets(layout);

    Private::RAIIIsRestoring isRestoring;

    // Hide all dockwidgets and unparent them from any layout before starting restore
    // We only close the stuff that the loaded JSON knows about. Unknown widgets might be newer.

    d->m_dockRegistry->clear(d->m_dockRegistry->dockWidgets(layout.dockWidgetsToClose()),
                             d->m_dockRegistry->mainWindows(layout.mainWindowNames()),
                             d->m_affinityNames);

    // 1. Restore main windows
    for (const LayoutSaver::MainWindow &mw : std::as_const(layout.mainWindows)) {
        MainWindowBase *mainWindow = d->m_dockRegistry->mainWindowByName(mw.uniqueName);
        if (!mainWindow) {
            if (auto mwFunc = Config::self().mainWindowFactoryFunc()) {
                mainWindow = mwFunc(mw.uniqueName);
            } else {
                qWarning() << "Failed to restore layout create MainWindow with name" << mw.uniqueName << "first";
                return false;
            }
        }

        if (!d->matchesAffinity(mainWindow->affinities()))
            continue;

        if (!(d->m_restoreOptions & InternalRestoreOption::SkipMainWindowGeometry)) {
            d->deserializeWindowGeometry(mw, mainWindow->window()); // window(), as the MainWindow can be embedded
            if (mw.windowState != Qt::WindowNoState) {
                if (auto w = mainWindow->windowHandle()) {
                    w->setWindowState(mw.windowState);
                }
            }
        }

        if (!mainWindow->deserialize(mw))
            return false;
    }

    // 2. Restore FloatingWindows
    for (LayoutSaver::FloatingWindow &fw : layout.floatingWindows) {
        if (!d->matchesAffinity(fw.affinities) || fw.skipsRestore())
            continue;

        MainWindowBase *parent = fw.parentIndex == -1 ? nullptr
                                                      : DockRegistry::self()->mainwindows().at(fw.parentIndex);

        auto floatingWindow = Config::self().frameworkWidgetFactory()->createFloatingWindow(parent);
        fw.floatingWindowInstance = floatingWindow;
        d->deserializeWindowGeometry(fw, floatingWindow);
        if (!floatingWindow->deserialize(fw)) {
            qWarning() << Q_FUNC_INFO << "Failed to deserialize floating window";
            return false;
        }
    }

    // 3. Restore closed dock widgets. They remain closed but acquire geometry and placeholder properties
    for (const auto &dw : std::as_const(layout.closedDockWidgets)) {
        if (d->matchesAffinity(dw->affinities)) {
            DockWidgetBase::deserialize(dw);
        }
    }

    // 4. Restore the placeholder info, now that the Items have been created
    for (const auto &dw : std::as_const(layout.allDockWidgets)) {
        if (!d->matchesAffinity(dw->affinities))
            continue;

        if (DockWidgetBase *dockWidget =
                d->m_dockRegistry->dockByName(dw->uniqueName, DockRegistry::DockByNameFlag::ConsultRemapping)) {
            dockWidget->d->lastPositions().deserialize(dw->lastPosition);
        } else {
            qWarning() << Q_FUNC_INFO << "Couldn't find dock widget" << dw->uniqueName;
        }
    }

    return true;
}

void LayoutSaver::setAffinityNames(const QStringList &affinityNames)
{
    d->m_affinityNames = affinityNames;
    if (affinityNames.contains(QString())) {
        // Any window with empty affinity will also be subject to save/restore
        d->m_affinityNames << QString();
    }
}

LayoutSaver::Private *LayoutSaver::dptr() const
{
    return d;
}

DockWidgetBase::List LayoutSaver::restoredDockWidgets() const
{
    const DockWidgetBase::List &allDockWidgets = DockRegistry::self()->dockwidgets();
    DockWidgetBase::List result;
    result.reserve(allDockWidgets.size());
    for (DockWidgetBase *dw : allDockWidgets) {
        if (dw->property("kddockwidget_was_restored").toBool())
            result.push_back(dw);
    }

    return result;
}

void LayoutSaver::Private::clearRestoredProperty()
{
    const DockWidgetBase::List &allDockWidgets = DockRegistry::self()->dockwidgets();
    for (DockWidgetBase *dw : allDockWidgets) {
        dw->setProperty("kddockwidget_was_restored", QVariant());
    }
}

template<typename T>
void LayoutSaver::Private::deserializeWindowGeometry(const T &saved, QWidgetOrQuick *topLevel)
{
    // Not simply calling QWidget::setGeometry() here.
    // For QtQuick we need to modify the QWindow's geometry.

    QRect geometry = saved.geometry;
    if (!isNormalWindowState(saved.windowState)) {
        // The window will be maximized. We first set its geometry to normal
        // Later it's maximized and will remember this value
        geometry = saved.normalGeometry;
    }

    ::FloatingWindow::ensureRectIsOnScreen(geometry);

    if (topLevel->isWindow()) {
        topLevel->setGeometry(geometry);
    } else {
        KDDockWidgets::Private::setTopLevelGeometry(geometry, topLevel);
    }

    topLevel->setVisible(saved.isVisible);
}

LayoutSaver::Private::Private(RestoreOptions options)
    : m_dockRegistry(DockRegistry::self())
    , m_restoreOptions(internalRestoreOptions(options))
{
}

bool LayoutSaver::Private::matchesAffinity(const QStringList &affinities) const
{
    return m_affinityNames.isEmpty() || affinities.isEmpty()
        || DockRegistry::self()->affinitiesMatch(m_affinityNames, affinities);
}

void LayoutSaver::Private::floatWidgetsWhichSkipRestore(const QStringList &mainWindowNames)
{
    // Widgets with the DockWidget::LayoutSaverOption::Skip flag skip restore completely.
    // If they were visible before they need to remain visible now.
    // If they were previously docked we need to float them, as the main window they were on will
    // be loading a new layout.

    for (MainWindowBase *mw : DockRegistry::self()->mainWindows(mainWindowNames)) {
        const KDDockWidgets::DockWidgetBase::List docks = mw->layoutWidget()->dockWidgets();
        for (auto dw : docks) {
            if (dw->skipsRestore()) {
                dw->setFloating(true);
            }
        }
    }
}

void LayoutSaver::Private::floatUnknownWidgets(const LayoutSaver::Layout &layout)
{
    // An old *.json layout file might have not know about existing dock widgets
    // When restoring such a file, we need to float any visible dock widgets which it doesn't know about
    // so we can restore the MainWindow layout properly

    for (MainWindowBase *mw : DockRegistry::self()->mainWindows(layout.mainWindowNames())) {
        const KDDockWidgets::DockWidgetBase::List docks = mw->layoutWidget()->dockWidgets();
        for (DockWidgetBase *dw : docks) {
            if (!layout.containsDockWidget(dw->uniqueName())) {
                dw->setFloating(true);
            }
        }
    }
}

void LayoutSaver::Private::deleteEmptyFrames()
{
    // After a restore it can happen that some DockWidgets didn't exist, so weren't restored.
    // Delete their frame now.

    for (auto frame : m_dockRegistry->frames()) {
        if (!frame->beingDeletedLater() && frame->isEmpty() && !frame->isCentralFrame())
            delete frame;
    }
}

std::unique_ptr<QSettings> LayoutSaver::Private::settings() const
{
    auto settings = std::unique_ptr<QSettings>(new QSettings(qApp->organizationName(),
                                                             qApp->applicationName()));
    settings->beginGroup(QStringLiteral("KDDockWidgets::LayoutSaver"));

    return settings;
}

bool LayoutSaver::restoreInProgress()
{
    return Private::s_restoreInProgress;
}

bool LayoutSaver::Layout::isValid() const
{
    if (serializationVersion != KDDOCKWIDGETS_SERIALIZATION_VERSION) {
        qWarning() << Q_FUNC_INFO << "Serialization format is too old"
                   << serializationVersion << "current=" << KDDOCKWIDGETS_SERIALIZATION_VERSION;
        return false;
    }

    for (auto &m : mainWindows) {
        if (!m.isValid())
            return false;
    }

    for (auto &m : floatingWindows) {
        if (!m.isValid())
            return false;
    }

    for (auto &m : allDockWidgets) {
        if (!m->isValid())
            return false;
    }

    return true;
}

QByteArray LayoutSaver::Layout::toJson() const
{
    QJsonDocument doc = QJsonDocument::fromVariant(toVariantMap());
    return doc.toJson();
}

bool LayoutSaver::Layout::fromJson(const QByteArray &jsonData)
{
    QJsonParseError error;
    QJsonDocument doc = QJsonDocument::fromJson(jsonData, &error);
    if (error.error == QJsonParseError::NoError) {
        fromVariantMap(doc.toVariant().toMap());
        return true;
    }

    return false;
}

QVariantMap LayoutSaver::Layout::toVariantMap() const
{
    QVariantMap map;
    map.insert(QStringLiteral("serializationVersion"), serializationVersion);
    map.insert(QStringLiteral("mainWindows"), toVariantList<LayoutSaver::MainWindow>(mainWindows));
    map.insert(QStringLiteral("floatingWindows"), toVariantList<LayoutSaver::FloatingWindow>(floatingWindows));
    map.insert(QStringLiteral("closedDockWidgets"), ::dockWidgetNames(closedDockWidgets));
    map.insert(QStringLiteral("allDockWidgets"), toVariantList(allDockWidgets));
    map.insert(QStringLiteral("screenInfo"), toVariantList<LayoutSaver::ScreenInfo>(screenInfo));

    return map;
}

void LayoutSaver::Layout::fromVariantMap(const QVariantMap &map)
{
    allDockWidgets.clear();
    const QVariantList dockWidgetsV = map.value(QStringLiteral("allDockWidgets")).toList();
    for (const QVariant &v : dockWidgetsV) {
        const QVariantMap dwV = v.toMap();
        const QString name = dwV.value(QStringLiteral("uniqueName")).toString();
        auto dw = LayoutSaver::DockWidget::dockWidgetForName(name);
        dw->fromVariantMap(dwV);
        allDockWidgets.push_back(dw);
    }

    closedDockWidgets.clear();
    const QVariantList closedDockWidgetsV = map.value(QStringLiteral("closedDockWidgets")).toList();
    closedDockWidgets.reserve(closedDockWidgetsV.size());
    for (const QVariant &v : closedDockWidgetsV) {
        closedDockWidgets.push_back(LayoutSaver::DockWidget::dockWidgetForName(v.toString()));
    }

    serializationVersion = map.value(QStringLiteral("serializationVersion")).toInt();
    mainWindows = fromVariantList<LayoutSaver::MainWindow>(map.value(QStringLiteral("mainWindows")).toList());
    floatingWindows = fromVariantList<LayoutSaver::FloatingWindow>(map.value(QStringLiteral("floatingWindows")).toList());
    screenInfo = fromVariantList<LayoutSaver::ScreenInfo>(map.value(QStringLiteral("screenInfo")).toList());
}

void LayoutSaver::Layout::scaleSizes(InternalRestoreOptions options)
{
    if (mainWindows.isEmpty())
        return;

    const bool skipsMainWindowGeometry = options & InternalRestoreOption::SkipMainWindowGeometry;
    if (!skipsMainWindowGeometry) {
        // No scaling to do. All windows will be restored with the exact size specified in the
        // saved JSON layouts.
        return;
    }

    // We won't restore MainWindow's geometry, we use whatever the user has now, meaning
    // we need to scale all dock widgets inside the layout, as the layout might not have
    // the same size as specified in the saved JSON layout
    for (auto &mw : mainWindows)
        mw.scaleSizes();


    // MainWindow has a different size than the one in JSON, so we also restore FloatingWindows
    // relatively to the user set new MainWindow size
    const bool useRelativeSizesForFloatingWidgets =
        options & InternalRestoreOption::RelativeFloatingWindowGeometry;

    if (useRelativeSizesForFloatingWidgets) {
        for (auto &fw : floatingWindows) {
            LayoutSaver::MainWindow mw = mainWindowForIndex(fw.parentIndex);
            if (mw.scalingInfo.isValid())
                fw.scaleSizes(mw.scalingInfo);
        }
    }

    const ScalingInfo firstScalingInfo = mainWindows.constFirst().scalingInfo;
    if (firstScalingInfo.isValid()) {
        for (auto &dw : allDockWidgets) {
            // TODO: Determine the best main window. This only interesting for closed dock
            // widget geometry which was previously floating. But they still have some other
            // main window as parent.
            dw->scaleSizes(firstScalingInfo);
        }
    }
}

LayoutSaver::MainWindow LayoutSaver::Layout::mainWindowForIndex(int index) const
{
    if (index < 0 || index >= mainWindows.size())
        return {};

    return mainWindows.at(index);
}

LayoutSaver::FloatingWindow LayoutSaver::Layout::floatingWindowForIndex(int index) const
{
    if (index < 0 || index >= floatingWindows.size())
        return {};

    return floatingWindows.at(index);
}

QStringList LayoutSaver::Layout::mainWindowNames() const
{
    QStringList names;
    names.reserve(mainWindows.size());
    for (const auto &mw : mainWindows) {
        names << mw.uniqueName;
    }

    return names;
}

QStringList LayoutSaver::Layout::dockWidgetNames() const
{
    QStringList names;
    names.reserve(allDockWidgets.size());
    for (const auto &dw : allDockWidgets) {
        names << dw->uniqueName;
    }

    return names;
}

QStringList LayoutSaver::Layout::dockWidgetsToClose() const
{
    // Before restoring a layout we close all dock widgets, unless they're a floating window with the DontCloseBeforeRestore flag

    QStringList names;
    names.reserve(allDockWidgets.size());
    auto registry = DockRegistry::self();
    for (const auto &dw : allDockWidgets) {
        if (DockWidgetBase *dockWidget = registry->dockByName(dw->uniqueName)) {

            bool doClose = true;

            if (dockWidget->skipsRestore()) {
                if (auto fw = dockWidget->floatingWindow()) {
                    if (fw->allDockWidgetsHave(DockWidgetBase::LayoutSaverOption::Skip)) {
                        // All dock widgets in this floating window skips float, so we can honour it for all.
                        doClose = false;
                    }
                }
            }

            if (doClose)
                names << dw->uniqueName;
        }
    }

    return names;
}

bool LayoutSaver::Layout::containsDockWidget(const QString &uniqueName) const
{
    return std::find_if(allDockWidgets.cbegin(), allDockWidgets.cend(), [uniqueName](const std::shared_ptr<LayoutSaver::DockWidget> &dock) {
               return dock->uniqueName == uniqueName;
           })
        != allDockWidgets.cend();
}

bool LayoutSaver::Frame::isValid() const
{
    if (isNull)
        return true;

    if (!geometry.isValid()) {
        qWarning() << Q_FUNC_INFO << "Invalid geometry";
        return false;
    }

    if (id.isEmpty()) {
        qWarning() << Q_FUNC_INFO << "Invalid id";
        return false;
    }

    if (!dockWidgets.isEmpty()) {
        if (currentTabIndex >= dockWidgets.size() || currentTabIndex < 0) {
            qWarning() << Q_FUNC_INFO << "Invalid tab index" << currentTabIndex << dockWidgets.size();
            return false;
        }
    }

    for (auto &dw : dockWidgets) {
        if (!dw->isValid())
            return false;
    }

    return true;
}

bool LayoutSaver::Frame::hasSingleDockWidget() const
{
    return dockWidgets.size() == 1;
}

bool LayoutSaver::Frame::skipsRestore() const
{
    return std::all_of(dockWidgets.cbegin(), dockWidgets.cend(), [](LayoutSaver::DockWidget::Ptr dw) {
        return dw->skipsRestore();
    });
}

LayoutSaver::DockWidget::Ptr LayoutSaver::Frame::singleDockWidget() const
{
    if (!hasSingleDockWidget())
        return {};

    return dockWidgets.first();
}

QVariantMap LayoutSaver::Frame::toVariantMap() const
{
    QVariantMap map;
    map.insert(QStringLiteral("id"), id);
    map.insert(QStringLiteral("isNull"), isNull);
    map.insert(QStringLiteral("objectName"), objectName);
    map.insert(QStringLiteral("geometry"), Layouting::rectToMap(geometry));
    map.insert(QStringLiteral("options"), options);
    map.insert(QStringLiteral("currentTabIndex"), currentTabIndex);
    map.insert(QStringLiteral("mainWindowUniqueName"), mainWindowUniqueName);
    map.insert(QStringLiteral("dockWidgets"), dockWidgetNames(dockWidgets));

    return map;
}

void LayoutSaver::Frame::fromVariantMap(const QVariantMap &map)
{
    if (map.isEmpty()) {
        isNull = true;
        dockWidgets.clear();
        return;
    }

    id = map.value(QStringLiteral("id")).toString();
    isNull = map.value(QStringLiteral("isNull")).toBool();
    objectName = map.value(QStringLiteral("objectName")).toString();
    mainWindowUniqueName = map.value(QStringLiteral("mainWindowUniqueName")).toString();
    geometry = Layouting::mapToRect(map.value(QStringLiteral("geometry")).toMap());
    options = static_cast<QFlags<FrameOption>::Int>(map.value(QStringLiteral("options")).toUInt());
    currentTabIndex = map.value(QStringLiteral("currentTabIndex")).toInt();

    const QVariantList dockWidgetsV = map.value(QStringLiteral("dockWidgets")).toList();

    dockWidgets.clear();
    dockWidgets.reserve(dockWidgetsV.size());
    for (const auto &variant : dockWidgetsV) {
        DockWidget::Ptr dw = DockWidget::dockWidgetForName(variant.toString());
        dockWidgets.push_back(dw);
    }
}

bool LayoutSaver::DockWidget::isValid() const
{
    return !uniqueName.isEmpty();
}

void LayoutSaver::DockWidget::scaleSizes(const ScalingInfo &scalingInfo)
{
    lastPosition.scaleSizes(scalingInfo);
}

bool LayoutSaver::DockWidget::skipsRestore() const
{
    if (DockWidgetBase *dw = DockRegistry::self()->dockByName(uniqueName))
        return dw->skipsRestore();

    return false;
}

QVariantMap LayoutSaver::DockWidget::toVariantMap() const
{
    QVariantMap map;
    if (!affinities.isEmpty())
        map.insert(QStringLiteral("affinities"), stringListToVariant(affinities));
    map.insert(QStringLiteral("uniqueName"), uniqueName);
    map.insert(QStringLiteral("lastPosition"), lastPosition.toVariantMap());

    return map;
}

void LayoutSaver::DockWidget::fromVariantMap(const QVariantMap &map)
{
    affinities = variantToStringList(map.value(QStringLiteral("affinities")).toList());

    // Compatibility hack. Old json format had a single "affinityName" instead of an "affinities" list:
    const QString affinityName = map.value(QStringLiteral("affinityName")).toString();
    if (!affinityName.isEmpty() && !affinities.contains(affinityName)) {
        affinities.push_back(affinityName);
    }

    uniqueName = map.value(QStringLiteral("uniqueName")).toString();
    lastPosition.fromVariantMap(map.value(QStringLiteral("lastPosition")).toMap());
}

bool LayoutSaver::FloatingWindow::isValid() const
{
    if (!multiSplitterLayout.isValid())
        return false;

    if (!geometry.isValid()) {
        qWarning() << Q_FUNC_INFO << "Invalid geometry";
        return false;
    }

    return true;
}

bool LayoutSaver::FloatingWindow::hasSingleDockWidget() const
{
    return multiSplitterLayout.hasSingleDockWidget();
}

LayoutSaver::DockWidget::Ptr LayoutSaver::FloatingWindow::singleDockWidget() const
{
    return multiSplitterLayout.singleDockWidget();
}

bool LayoutSaver::FloatingWindow::skipsRestore() const
{
    return multiSplitterLayout.skipsRestore();
}

void LayoutSaver::FloatingWindow::scaleSizes(const ScalingInfo &scalingInfo)
{
    scalingInfo.applyFactorsTo(/*by-ref*/ geometry);
}

QVariantMap LayoutSaver::FloatingWindow::toVariantMap() const
{
    QVariantMap map;
    map.insert(QStringLiteral("multiSplitterLayout"), multiSplitterLayout.toVariantMap());
    map.insert(QStringLiteral("parentIndex"), parentIndex);
    map.insert(QStringLiteral("geometry"), Layouting::rectToMap(geometry));
    map.insert(QStringLiteral("normalGeometry"), Layouting::rectToMap(normalGeometry));
    map.insert(QStringLiteral("screenIndex"), screenIndex);
    map.insert(QStringLiteral("screenSize"), Layouting::sizeToMap(screenSize));
    map.insert(QStringLiteral("isVisible"), isVisible);
    map.insert(QStringLiteral("windowState"), windowState);

    if (!affinities.isEmpty())
        map.insert(QStringLiteral("affinities"), stringListToVariant(affinities));

    return map;
}

void LayoutSaver::FloatingWindow::fromVariantMap(const QVariantMap &map)
{
    multiSplitterLayout.fromVariantMap(map.value(QStringLiteral("multiSplitterLayout")).toMap());
    parentIndex = map.value(QStringLiteral("parentIndex")).toInt();
    geometry = Layouting::mapToRect(map.value(QStringLiteral("geometry")).toMap());
    normalGeometry = Layouting::mapToRect(map.value(QStringLiteral("normalGeometry")).toMap());
    screenIndex = map.value(QStringLiteral("screenIndex")).toInt();
    screenSize = Layouting::mapToSize(map.value(QStringLiteral("screenSize")).toMap());
    isVisible = map.value(QStringLiteral("isVisible")).toBool();
    affinities = variantToStringList(map.value(QStringLiteral("affinities")).toList());
    windowState = Qt::WindowState(map.value(QStringLiteral("windowState"), Qt::WindowNoState).toInt());

    // Compatibility hack. Old json format had a single "affinityName" instead of an "affinities" list:
    const QString affinityName = map.value(QStringLiteral("affinityName")).toString();
    if (!affinityName.isEmpty() && !affinities.contains(affinityName)) {
        affinities.push_back(affinityName);
    }
}

bool LayoutSaver::MainWindow::isValid() const
{
    if (!multiSplitterLayout.isValid())
        return false;

    return true;
}

void LayoutSaver::MainWindow::scaleSizes()
{
    if (scalingInfo.isValid()) {
        // Doesn't happen, it's called only once
        Q_ASSERT(false);
        return;
    }

    scalingInfo = ScalingInfo(uniqueName, geometry, screenIndex);
}

QVariantMap LayoutSaver::MainWindow::toVariantMap() const
{
    QVariantMap map;
    map.insert(QStringLiteral("options"), int(options));
    map.insert(QStringLiteral("multiSplitterLayout"), multiSplitterLayout.toVariantMap());
    map.insert(QStringLiteral("uniqueName"), uniqueName);
    map.insert(QStringLiteral("geometry"), Layouting::rectToMap(geometry));
    map.insert(QStringLiteral("normalGeometry"), Layouting::rectToMap(normalGeometry));
    map.insert(QStringLiteral("screenIndex"), screenIndex);
    map.insert(QStringLiteral("screenSize"), Layouting::sizeToMap(screenSize));
    map.insert(QStringLiteral("isVisible"), isVisible);
    map.insert(QStringLiteral("affinities"), stringListToVariant(affinities));
    map.insert(QStringLiteral("windowState"), windowState);

    for (SideBarLocation loc : { SideBarLocation::North, SideBarLocation::East, SideBarLocation::West, SideBarLocation::South }) {
        const QStringList dockWidgets = dockWidgetsPerSideBar.value(loc);
        if (!dockWidgets.isEmpty())
            map.insert(QStringLiteral("sidebar-%1").arg(int(loc)), stringListToVariant(dockWidgets));
    }

    return map;
}

void LayoutSaver::MainWindow::fromVariantMap(const QVariantMap &map)
{
    options = KDDockWidgets::MainWindowOptions(map.value(QStringLiteral("options")).toInt());
    multiSplitterLayout.fromVariantMap(map.value(QStringLiteral("multiSplitterLayout")).toMap());
    uniqueName = map.value(QStringLiteral("uniqueName")).toString();
    geometry = Layouting::mapToRect(map.value(QStringLiteral("geometry")).toMap());
    normalGeometry = Layouting::mapToRect(map.value(QStringLiteral("normalGeometry")).toMap());
    screenIndex = map.value(QStringLiteral("screenIndex")).toInt();
    screenSize = Layouting::mapToSize(map.value(QStringLiteral("screenSize")).toMap());
    isVisible = map.value(QStringLiteral("isVisible")).toBool();
    affinities = variantToStringList(map.value(QStringLiteral("affinities")).toList());
    windowState = Qt::WindowState(map.value(QStringLiteral("windowState"), Qt::WindowNoState).toInt());

    // Compatibility hack. Old json format had a single "affinityName" instead of an "affinities" list:
    const QString affinityName = map.value(QStringLiteral("affinityName")).toString();
    if (!affinityName.isEmpty() && !affinities.contains(affinityName)) {
        affinities.push_back(affinityName);
    }

    // Load the SideBars:
    dockWidgetsPerSideBar.clear();
    for (SideBarLocation loc : { SideBarLocation::North, SideBarLocation::East, SideBarLocation::West, SideBarLocation::South }) {
        const QVariantList dockWidgets = map.value(QStringLiteral("sidebar-%1").arg(int(loc))).toList();
        if (!dockWidgets.isEmpty())
            dockWidgetsPerSideBar.insert(loc, variantToStringList(dockWidgets));
    }
}

bool LayoutSaver::MultiSplitter::isValid() const
{
    if (layout.isEmpty())
        return false;

    /*if (!size.isValid()) {
        qWarning() << Q_FUNC_INFO << "Invalid size";
        return false;
    }*/

    return true;
}

bool LayoutSaver::MultiSplitter::hasSingleDockWidget() const
{
    return frames.size() == 1 && frames.cbegin()->hasSingleDockWidget();
}

LayoutSaver::DockWidget::Ptr LayoutSaver::MultiSplitter::singleDockWidget() const
{
    if (!hasSingleDockWidget())
        return {};

    return frames.cbegin()->singleDockWidget();
}

bool LayoutSaver::MultiSplitter::skipsRestore() const
{
    return std::all_of(frames.cbegin(), frames.cend(), [](const LayoutSaver::Frame &frame) {
        return frame.skipsRestore();
    });
}

QVariantMap LayoutSaver::MultiSplitter::toVariantMap() const
{
    QVariantMap result;
    result.insert(QStringLiteral("layout"), layout);

    QVariantMap framesV;
    for (auto &frame : frames)
        framesV.insert(frame.id, frame.toVariantMap());

    result.insert(QStringLiteral("frames"), framesV);
    return result;
}

void LayoutSaver::MultiSplitter::fromVariantMap(const QVariantMap &map)
{
    layout = map.value(QStringLiteral("layout")).toMap();
    const QVariantMap framesV = map.value(QStringLiteral("frames")).toMap();
    frames.clear();
    for (const QVariant &frameV : framesV) {
        LayoutSaver::Frame frame;
        frame.fromVariantMap(frameV.toMap());
        frames.insert(frame.id, frame);
    }
}

void LayoutSaver::Position::scaleSizes(const ScalingInfo &scalingInfo)
{
    scalingInfo.applyFactorsTo(/*by-ref*/ lastFloatingGeometry);
}

QVariantMap LayoutSaver::Position::toVariantMap() const
{
    QVariantMap map;
    map.insert(QStringLiteral("lastFloatingGeometry"), Layouting::rectToMap(lastFloatingGeometry));
    map.insert(QStringLiteral("tabIndex"), tabIndex);
    map.insert(QStringLiteral("wasFloating"), wasFloating);
    map.insert(QStringLiteral("placeholders"), toVariantList<LayoutSaver::Placeholder>(placeholders));

    return map;
}

void LayoutSaver::Position::fromVariantMap(const QVariantMap &map)
{
    lastFloatingGeometry = Layouting::mapToRect(map.value(QStringLiteral("lastFloatingGeometry")).toMap());
    tabIndex = map.value(QStringLiteral("tabIndex")).toInt();
    wasFloating = map.value(QStringLiteral("wasFloating")).toBool();
    placeholders = fromVariantList<LayoutSaver::Placeholder>(map.value(QStringLiteral("placeholders")).toList());
}

QVariantMap LayoutSaver::ScreenInfo::toVariantMap() const
{
    QVariantMap map;
    map.insert(QStringLiteral("index"), index);
    map.insert(QStringLiteral("geometry"), Layouting::rectToMap(geometry));
    map.insert(QStringLiteral("name"), name);
    map.insert(QStringLiteral("devicePixelRatio"), devicePixelRatio);

    return map;
}

void LayoutSaver::ScreenInfo::fromVariantMap(const QVariantMap &map)
{
    index = map.value(QStringLiteral("index")).toInt();
    geometry = Layouting::mapToRect(map.value(QStringLiteral("geometry")).toMap());
    name = map.value(QStringLiteral("name")).toString();
    devicePixelRatio = map.value(QStringLiteral("devicePixelRatio")).toDouble();
}

QVariantMap LayoutSaver::Placeholder::toVariantMap() const
{
    QVariantMap map;
    map.insert(QStringLiteral("isFloatingWindow"), isFloatingWindow);
    map.insert(QStringLiteral("itemIndex"), itemIndex);

    if (isFloatingWindow)
        map.insert(QStringLiteral("indexOfFloatingWindow"), indexOfFloatingWindow);
    else
        map.insert(QStringLiteral("mainWindowUniqueName"), mainWindowUniqueName);

    return map;
}

void LayoutSaver::Placeholder::fromVariantMap(const QVariantMap &map)
{
    isFloatingWindow = map.value(QStringLiteral("isFloatingWindow")).toBool();
    indexOfFloatingWindow = map.value(QStringLiteral("indexOfFloatingWindow"), -1).toInt();
    itemIndex = map.value(QStringLiteral("itemIndex")).toInt();
    mainWindowUniqueName = map.value(QStringLiteral("mainWindowUniqueName")).toString();
}

LayoutSaver::ScalingInfo::ScalingInfo(const QString &mainWindowId, QRect savedMainWindowGeo, int screenIndex)
{
    auto mainWindow = DockRegistry::self()->mainWindowByName(mainWindowId);
    if (!mainWindow) {
        qWarning() << Q_FUNC_INFO << "Failed to find main window with name" << mainWindowName;
        return;
    }

    if (!savedMainWindowGeo.isValid() || savedMainWindowGeo.isNull()) {
        qWarning() << Q_FUNC_INFO << "Invalid saved main window geometry" << savedMainWindowGeo;
        return;
    }

    if (!mainWindow->geometry().isValid() || mainWindow->geometry().isNull()) {
        qWarning() << Q_FUNC_INFO << "Invalid main window geometry" << mainWindow->geometry();
        return;
    }

    const int currentScreenIndex = qApp->screens().indexOf(mainWindow->screen());

    this->mainWindowName = mainWindowId;
    this->savedMainWindowGeometry = savedMainWindowGeo;
    realMainWindowGeometry = mainWindow->window()->geometry(); // window() as our main window might be embedded
    widthFactor = double(realMainWindowGeometry.width()) / savedMainWindowGeo.width();
    heightFactor = double(realMainWindowGeometry.height()) / savedMainWindowGeo.height();
    mainWindowChangedScreen = currentScreenIndex != screenIndex;
}

void LayoutSaver::ScalingInfo::translatePos(QPoint &pt) const
{
    const int deltaX = pt.x() - savedMainWindowGeometry.x();
    const int deltaY = pt.y() - savedMainWindowGeometry.y();

    const double newDeltaX = deltaX * widthFactor;
    const double newDeltaY = deltaY * heightFactor;

    pt.setX(qCeil(savedMainWindowGeometry.x() + newDeltaX));
    pt.setY(qCeil(savedMainWindowGeometry.y() + newDeltaY));
}

void LayoutSaver::ScalingInfo::applyFactorsTo(QPoint &pt) const
{
    translatePos(pt);
}

void LayoutSaver::ScalingInfo::applyFactorsTo(QSize &sz) const
{
    sz.setWidth(int(widthFactor * sz.width()));
    sz.setHeight(int(heightFactor * sz.height()));
}

void LayoutSaver::ScalingInfo::applyFactorsTo(QRect &rect) const
{
    if (rect.isEmpty())
        return;

    QPoint pos = rect.topLeft();
    QSize size = rect.size();

    applyFactorsTo(/*by-ref*/ size);


    if (!mainWindowChangedScreen) {
        // Don't play with floating window position if the main window changed screen.
        // There's too many corner cases that push the floating windows out of bounds, and
        // we're not even considering monitors with different HDPI. We can support only the simple case.
        // For complex cases we'll try to guarantee the window is placed somewhere reasonable.
        applyFactorsTo(/*by-ref*/ pos);
    }

    rect.moveTopLeft(pos);
    rect.setSize(size);
}

LayoutSaver::Private::RAIIIsRestoring::RAIIIsRestoring()
{
    LayoutSaver::Private::s_restoreInProgress = true;
}

LayoutSaver::Private::RAIIIsRestoring::~RAIIIsRestoring()
{
    LayoutSaver::Private::s_restoreInProgress = false;
}
