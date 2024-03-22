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
#include "interactiveprovider.h"

#include <QMetaType>
#include <QMetaProperty>
#include <QDialog>
#include <QFileDialog>
#include <QGuiApplication>
#include <QWindow>

#include "containers.h"
#include "log.h"

using namespace mu;
using namespace mu::ui;

class WidgetDialogEventFilter : public QObject
{
public:
    WidgetDialogEventFilter(QObject* parent, const std::function<void()>& onShownCallBack,
                            const std::function<void()>& onHideCallBack = std::function<void()>())
        : QObject(parent), m_onShownCallBack(onShownCallBack), m_onHideCallBack(onHideCallBack)
    {
    }

private:
    bool eventFilter(QObject* watched, QEvent* event) override
    {
        if (event->type() == QEvent::Show && m_onShownCallBack) {
            m_onShownCallBack();
        }

        if (event->type() == QEvent::Hide && m_onHideCallBack) {
            m_onHideCallBack();
        }

        return QObject::eventFilter(watched, event);
    }

    std::function<void()> m_onShownCallBack;
    std::function<void()> m_onHideCallBack;
};

InteractiveProvider::InteractiveProvider()
    : QObject()
{
    connect(qApp, &QGuiApplication::focusWindowChanged, this, [this](QWindow* window) {
        raiseWindowInStack(window);
    });

    connect(qApp, &QGuiApplication::focusObjectChanged, this, [this](QObject* obj) {
        auto widget = dynamic_cast<QWidget*>(obj);

        if (widget && widget->isWindow()) {
            raiseWindowInStack(widget);
        }
    });
}

void InteractiveProvider::raiseWindowInStack(QObject* newActiveWindow)
{
    if (!newActiveWindow || m_stack.isEmpty() || m_stack.top().window == newActiveWindow) {
        return;
    }

    for (int i = 0; i < m_stack.size(); ++i) {
        bool found = m_stack[i].window == newActiveWindow;
        if (m_stack[i].window->isWidgetType()) {
            found = newActiveWindow->objectName() == (m_stack[i].window->objectName() + "Window");
        }

        if (found) {
            ObjectInfo info = m_stack.takeAt(i);
            m_stack.push(info);
            notifyAboutCurrentUriChanged();
            return;
        }
    }
}

RetVal<Val> InteractiveProvider::question(const std::string& title, const IInteractive::Text& text,
                                          const IInteractive::ButtonDatas& buttons, int defBtn,
                                          const IInteractive::Options& options)
{
    return openStandardDialog("QUESTION", title, text, {}, buttons, defBtn, options);
}

RetVal<Val> InteractiveProvider::info(const std::string& title, const IInteractive::Text& text, const IInteractive::ButtonDatas& buttons,
                                      int defBtn,
                                      const IInteractive::Options& options)
{
    return openStandardDialog("INFO", title, text, {}, buttons, defBtn, options);
}

RetVal<Val> InteractiveProvider::warning(const std::string& title, const IInteractive::Text& text,
                                         const std::string& detailedText,
                                         const IInteractive::ButtonDatas& buttons,
                                         int defBtn,
                                         const IInteractive::Options& options)
{
    return openStandardDialog("WARNING", title, text, detailedText, buttons, defBtn, options);
}

RetVal<Val> InteractiveProvider::error(const std::string& title, const IInteractive::Text& text,
                                       const std::string& detailedText,
                                       const IInteractive::ButtonDatas& buttons,
                                       int defBtn,
                                       const IInteractive::Options& options)
{
    return openStandardDialog("ERROR", title, text, detailedText, buttons, defBtn, options);
}

Ret InteractiveProvider::showProgress(const std::string& title, mu::Progress* progress)
{
    IF_ASSERT_FAILED(progress) {
        return false;
    }

    QVariantMap params;
    params["title"] = QString::fromStdString(title);
    params["progress"] = QVariant::fromValue(progress);

    QmlLaunchData* data = new QmlLaunchData();
    data->setValue("params", params);

    emit fireOpenProgressDialog(data);

    Ret ret = toRet(data->value("ret"));
    QString objectId = data->value("objectId").toString();

    delete data;

    if (!ret) {
        return ret;
    }

    if (!objectId.isEmpty()) {
        RetVal<Val> rv = m_retvals.take(objectId);
        if (rv.ret.valid()) {
            return rv.ret;
        }
    }

    return make_ok();
}

RetVal<io::path_t> InteractiveProvider::selectOpeningFile(const std::string& title, const io::path_t& dir,
                                                          const std::vector<std::string>& filter)
{
    return openFileDialog(FileDialogType::SelectOpenningFile, title, dir, filter);
}

RetVal<io::path_t> InteractiveProvider::selectSavingFile(const std::string& title, const io::path_t& path,
                                                         const std::vector<std::string>& filter,
                                                         bool confirmOverwrite)
{
    return openFileDialog(FileDialogType::SelectSavingFile, title, path, filter, confirmOverwrite);
}

RetVal<io::path_t> InteractiveProvider::selectDirectory(const std::string& title, const io::path_t& dir)
{
    return openFileDialog(FileDialogType::SelectDirectory, title, dir);
}

RetVal<Val> InteractiveProvider::open(const UriQuery& q)
{
    m_openingUriQuery = q;
    RetVal<OpenData> openedRet;

    //! NOTE Currently, extensions do not replace the default functionality
    //! But in the future, we may allow extensions to replace the current functionality
    //! (first check for the presence of an extension with this uri,
    //! and if it is found, then open it)

    ContainerMeta openMeta = uriRegister()->meta(q.uri());
    switch (openMeta.type) {
    case ContainerType::QWidgetDialog:
        openedRet = openWidgetDialog(q);
        break;
    case ContainerType::PrimaryPage:
    case ContainerType::QmlDialog:
        openedRet = openQml(q);
        break;
    case ContainerType::Undefined: {
        //! NOTE Not found default, try extension
        extensions::Manifest ext = extensionsProvider()->manifest(q.uri());
        if (ext.isValid()) {
            openedRet = openExtensionDialog(q);
        } else {
            openedRet.ret = make_ret(Ret::Code::UnknownError);
        }
    }
    }

    if (!openedRet.ret) {
        LOGE() << "failed open err: " << openedRet.ret.toString() << ", uri: " << q.toString();
        return RetVal<Val>(openedRet.ret);
    }

    RetVal<Val> returnedRV;
    returnedRV.ret = make_ret(Ret::Code::Ok);
    if (openedRet.val.sync && !openedRet.val.objectId.isEmpty()) {
        RetVal<Val> rv = m_retvals.take(openedRet.val.objectId);
        if (rv.ret.valid()) {
            returnedRV = rv;
        }
    }

    return returnedRV;
}

RetVal<bool> InteractiveProvider::isOpened(const Uri& uri) const
{
    for (const ObjectInfo& objectInfo: allOpenObjects()) {
        if (objectInfo.uriQuery.uri() == uri) {
            return RetVal<bool>::make_ok(true);
        }
    }

    return RetVal<bool>::make_ok(false);
}

RetVal<bool> InteractiveProvider::isOpened(const UriQuery& uri) const
{
    for (const ObjectInfo& objectInfo: allOpenObjects()) {
        if (objectInfo.uriQuery == uri) {
            return RetVal<bool>::make_ok(true);
        }
    }

    return RetVal<bool>::make_ok(false);
}

async::Channel<Uri> InteractiveProvider::opened() const
{
    return m_opened;
}

void InteractiveProvider::raise(const UriQuery& uri)
{
    for (const ObjectInfo& objectInfo: allOpenObjects()) {
        if (objectInfo.uriQuery != uri) {
            continue;
        }

        ContainerMeta openMeta = uriRegister()->meta(objectInfo.uriQuery.uri());
        switch (openMeta.type) {
        case ContainerType::QWidgetDialog: {
            if (auto window = dynamic_cast<QWidget*>(objectInfo.window)) {
                window->raise();
                window->activateWindow();
            }
        } break;
        case ContainerType::QmlDialog:
            raiseQml(objectInfo.objectId);
            break;
        case ContainerType::PrimaryPage:
        case ContainerType::Undefined:
            break;
        }
    }
}

void InteractiveProvider::close(const Uri& uri)
{
    for (const ObjectInfo& obj : allOpenObjects()) {
        if (obj.uriQuery.uri() == uri) {
            closeObject(obj);
        }
    }
}

void InteractiveProvider::close(const UriQuery& uri)
{
    for (const ObjectInfo& obj : allOpenObjects()) {
        if (obj.uriQuery == uri) {
            closeObject(obj);
        }
    }
}

void InteractiveProvider::closeAllDialogs()
{
    for (const ObjectInfo& objectInfo: allOpenObjects()) {
        UriQuery uriQuery = objectInfo.uriQuery;
        ContainerMeta openMeta = uriRegister()->meta(uriQuery.uri());
        if (openMeta.type == ContainerType::QWidgetDialog || openMeta.type == ContainerType::QmlDialog) {
            closeObject(objectInfo);
        }
    }
}

void InteractiveProvider::closeObject(const ObjectInfo& obj)
{
    ContainerMeta openMeta = uriRegister()->meta(obj.uriQuery.uri());
    switch (openMeta.type) {
    case ContainerType::QWidgetDialog: {
        if (auto window = dynamic_cast<QWidget*>(obj.window)) {
            window->close();
        }
    } break;
    case ContainerType::QmlDialog:
        closeQml(obj.objectId);
        break;
    case ContainerType::PrimaryPage:
    case ContainerType::Undefined:
        break;
    }
}

void InteractiveProvider::fillExtData(QmlLaunchData* data, const UriQuery& q) const
{
    static Uri VIEWER_URI = Uri("musescore://extensions/viewer");

    ContainerMeta meta = uriRegister()->meta(VIEWER_URI);
    data->setValue("path", meta.qmlPath);
    data->setValue("type", meta.type);

    QVariantMap params;
    params["uri"] = QString::fromStdString(q.toString());

    data->setValue("sync", params.value("sync", false));
    data->setValue("modal", params.value("modal", ""));
    data->setValue("params", params);
}

void InteractiveProvider::fillData(QmlLaunchData* data, const UriQuery& q) const
{
    ContainerMeta meta = uriRegister()->meta(q.uri());
    data->setValue("path", meta.qmlPath);
    data->setValue("type", meta.type);
    data->setValue("uri", QString::fromStdString(q.uri().toString()));

    QVariantMap params;
    const UriQuery::Params& p = q.params();
    for (auto it = p.cbegin(); it != p.cend(); ++it) {
        params[QString::fromStdString(it->first)] = it->second.toQVariant();
    }

    data->setValue("params", params);
    data->setValue("sync", params.value("sync", false));
    data->setValue("modal", params.value("modal", ""));
}

void InteractiveProvider::fillData(QObject* object, const UriQuery& q) const
{
    QVariantMap params;
    const UriQuery::Params& p = q.params();
    for (auto it = p.cbegin(); it != p.cend(); ++it) {
        params[QString::fromStdString(it->first)] = it->second.toQVariant();
    }

    const QMetaObject* metaObject = object->metaObject();
    for (int i = 0; i < metaObject->propertyCount(); i++) {
        QMetaProperty metaProperty = metaObject->property(i);
        if (params.contains(metaProperty.name())) {
            object->setProperty(metaProperty.name(), params[metaProperty.name()]);
        }
    }
}

void InteractiveProvider::fillStandardDialogData(QmlLaunchData* data, const QString& type, const std::string& title,
                                                 const IInteractive::Text& text, const std::string& detailedText,
                                                 const IInteractive::ButtonDatas& buttons, int defBtn,
                                                 const IInteractive::Options& options) const
{
    auto format = [](IInteractive::TextFormat f) {
        switch (f) {
        case IInteractive::TextFormat::Auto:      return Qt::AutoText;
        case IInteractive::TextFormat::PlainText: return Qt::PlainText;
        case IInteractive::TextFormat::RichText:  return Qt::RichText;
        }
        return Qt::PlainText;
    };

    QVariantMap params;
    params["type"] = type;
    params["title"] = QString::fromStdString(title);
    params["text"] = QString::fromStdString(text.text);
    params["detailedText"] = QString::fromStdString(detailedText);
    params["textFormat"] = format(text.format);
    params["defaultButtonId"] = defBtn;

    QVariantList buttonsList;
    QVariantList customButtonsList;
    if (buttons.empty()) {
        buttonsList << static_cast<int>(IInteractive::Button::Ok);
    } else {
        for (const IInteractive::ButtonData& buttonData: buttons) {
            QVariantMap customButton;
            customButton["text"] = QString::fromStdString(buttonData.text);
            customButton["buttonId"] = buttonData.btn;
            customButton["role"] = static_cast<int>(buttonData.role);
            customButton["isAccent"] = buttonData.accent;
            customButton["isLeftSide"] = buttonData.leftSide;
            customButtonsList << QVariant(customButton);
        }
    }

    params["buttons"] = buttonsList;
    params["customButtons"] = customButtonsList;

    if (options.testFlag(IInteractive::Option::WithIcon)) {
        params["withIcon"] = true;
    }

    if (options.testFlag(IInteractive::Option::WithDontShowAgainCheckBox)) {
        params["withDontShowAgainCheckBox"] = true;
    }

    data->setValue("params", params);
}

void InteractiveProvider::fillFileDialogData(QmlLaunchData* data, FileDialogType type, const std::string& title, const io::path_t& path,
                                             const std::vector<std::string>& filter, bool confirmOverwrite) const
{
    QVariantMap params;
    params["title"] = QString::fromStdString(title);

    if (type == FileDialogType::SelectOpenningFile || type == FileDialogType::SelectSavingFile) {
        QStringList filterList;
        for (const std::string& nameFilter : filter) {
            filterList << QString::fromStdString(nameFilter);
        }

        params["nameFilters"] = filterList;
        params["selectExisting"] = type == FileDialogType::SelectOpenningFile;

        if (type == FileDialogType::SelectOpenningFile) {
            // see QQuickPlatformFileDialog::FileMode::OpenFile
            params["fileMode"] = 0;
            params["folder"] = QUrl::fromLocalFile(path.toQString());
        } else {
            // see QQuickPlatformFileDialog::FileMode::SaveFile
            params["fileMode"] = 2;
            params["currentFile"] = QUrl::fromLocalFile(path.toQString());

            if (!confirmOverwrite) {
                params["options"] = QFileDialog::DontConfirmOverwrite;
            }
        }
    }

    data->setValue("params", params);
    data->setValue("selectFolder", type == FileDialogType::SelectDirectory);
}

ValCh<Uri> InteractiveProvider::currentUri() const
{
    ValCh<Uri> v;
    if (!m_stack.empty()) {
        v.val = m_stack.last().uriQuery.uri();
    }
    v.ch = m_currentUriChanged;
    return v;
}

async::Notification InteractiveProvider::currentUriAboutToBeChanged() const
{
    return m_currentUriAboutToBeChanged;
}

std::vector<Uri> InteractiveProvider::stack() const
{
    std::vector<Uri> uris;
    for (const ObjectInfo& info : m_stack) {
        uris.push_back(info.uriQuery.uri());
    }
    return uris;
}

QWindow* InteractiveProvider::topWindow() const
{
    QWindow* mainWin = mainWindow()->qWindow();

    if (m_stack.empty()) {
        LOGE() << "stack is empty";
        return mainWin;
    }

    const ObjectInfo& last = m_stack.last();
    if (!last.window) {
        return mainWin;
    }

    if (last.window == mainWin) {
        return mainWin;
    }

    if (!last.window->parent()) {
        ASSERT_X("Window must have a parent!");
    }

    return qobject_cast<QWindow*>(last.window);
}

bool InteractiveProvider::topWindowIsWidget() const
{
    if (m_stack.empty()) {
        return false;
    }

    const ObjectInfo& last = m_stack.last();
    if (!last.window) {
        return false;
    }

    return last.window->isWidgetType();
}

QString InteractiveProvider::objectId(const QVariant& val) const
{
    static int count(0);

    ++count;

    QString objectId;
    if (val.canConvert<QObject*>()) {
        QObject* obj = val.value<QObject*>();
        IF_ASSERT_FAILED(obj) {
            return QString();
        }

        objectId = QString(obj->metaObject()->className());
    } else {
        objectId = "unknown_" + QString::number(count);
    }
    return "object://" + objectId;
}

Ret InteractiveProvider::toRet(const QVariant& jsr) const
{
    QVariantMap jsobj = jsr.toMap();
    IF_ASSERT_FAILED(jsobj.contains("errcode")) {
        return make_ret(Ret::Code::UnknownError);
    }

    Ret ret;
    ret.setCode(jsobj.value("errcode").toInt());
    return ret;
}

RetVal<Val> InteractiveProvider::toRetVal(const QVariant& jsrv) const
{
    RetVal<Val> rv;
    QVariantMap jsobj = jsrv.toMap();

    IF_ASSERT_FAILED(jsobj.contains("errcode")) {
        rv.ret = make_ret(Ret::Code::UnknownError);
        return rv;
    }

    int errcode = jsobj.value("errcode").toInt();
    QVariant val = jsobj.value("value");

    rv.ret.setCode(errcode);
    rv.val = Val::fromQVariant(val);

    return rv;
}

RetVal<InteractiveProvider::OpenData> InteractiveProvider::openExtensionDialog(const UriQuery& q)
{
    notifyAboutCurrentUriWillBeChanged();

    QmlLaunchData data;
    fillExtData(&data, q);

    emit fireOpen(&data);

    RetVal<OpenData> result;
    result.ret = toRet(data.value("ret"));
    result.val.sync = data.value("sync").toBool();
    result.val.objectId = data.value("objectId").toString();

    return result;
}

RetVal<InteractiveProvider::OpenData> InteractiveProvider::openWidgetDialog(const UriQuery& q)
{
    notifyAboutCurrentUriWillBeChanged();

    RetVal<OpenData> result;

    ContainerMeta meta = uriRegister()->meta(q.uri());
    int widgetMetaTypeId = meta.widgetMetaTypeId;

    static int count(0);
    QString objectId = QString("%1_%2").arg(widgetMetaTypeId).arg(++count);

    QMetaType metaType = QMetaType(widgetMetaTypeId);
    QDialog* dialog = static_cast<QDialog*>(metaType.create());

    if (!dialog) {
        result.ret = make_ret(Ret::Code::UnknownError);
        return result;
    }

    fillData(dialog, q);

    dialog->installEventFilter(new WidgetDialogEventFilter(dialog,
                                                           [this, dialog, objectId]() {
        if (dialog) {
            onOpen(ContainerType::QWidgetDialog, objectId, dialog->window());
        }
    },
                                                           [this, dialog, objectId]() {
        if (!dialog) {
            return;
        }

        QVariantMap status;

        QDialog::DialogCode dialogCode = static_cast<QDialog::DialogCode>(dialog->result());
        Ret::Code errorCode = dialogCode == QDialog::Accepted ? Ret::Code::Ok : Ret::Code::Cancel;
        status["errcode"] = static_cast<int>(errorCode);

        onClose(objectId, status);
        dialog->deleteLater();
    }));

    bool sync = q.param("sync", Val(false)).toBool();
    if (sync) {
        dialog->exec();
    } else {
        dialog->show();
        dialog->activateWindow(); // give keyboard focus to aid blind users
    }

    result.ret = make_ret(Ret::Code::Ok);
    result.val.sync = sync;
    result.val.objectId = objectId;

    return result;
}

RetVal<InteractiveProvider::OpenData> InteractiveProvider::openQml(const UriQuery& q)
{
    notifyAboutCurrentUriWillBeChanged();

    QmlLaunchData* data = new QmlLaunchData();
    fillData(data, q);

    emit fireOpen(data);

    RetVal<OpenData> result;
    result.ret = toRet(data->value("ret"));
    result.val.sync = data->value("sync").toBool();
    result.val.objectId = data->value("objectId").toString();

    delete data;

    return result;
}

RetVal<Val> InteractiveProvider::openStandardDialog(const QString& type, const std::string& title, const IInteractive::Text& text,
                                                    const std::string& detailedText,
                                                    const IInteractive::ButtonDatas& buttons, int defBtn,
                                                    const IInteractive::Options& options)
{
    notifyAboutCurrentUriWillBeChanged();

    QmlLaunchData* data = new QmlLaunchData();
    fillStandardDialogData(data, type, title, text, detailedText, buttons, defBtn, options);

    emit fireOpenStandardDialog(data);

    Ret ret = toRet(data->value("ret"));
    QString objectId = data->value("objectId").toString();

    delete data;

    RetVal<Val> result;
    if (!ret) {
        result.ret = ret;
        return result;
    }

    if (!objectId.isEmpty()) {
        RetVal<Val> rv = m_retvals.take(objectId);
        if (rv.ret.valid()) {
            result = rv;
        }
    }

    return result;
}

RetVal<io::path_t> InteractiveProvider::openFileDialog(FileDialogType type, const std::string& title, const io::path_t& path,
                                                       const std::vector<std::string>& filter, bool confirmOverwrite)
{
    notifyAboutCurrentUriWillBeChanged();

    RetVal<io::path_t> result;

    QmlLaunchData* data = new QmlLaunchData();
    fillFileDialogData(data, type, title, path, filter, confirmOverwrite);

    emit fireOpenFileDialog(data);

    m_fileDialogEventLoop.exec();

    QString objectId = data->value("objectId").toString();

    delete data;

    if (!objectId.isEmpty()) {
        RetVal<Val> rv = m_retvals.take(objectId);
        if (rv.ret.valid()) {
            result.ret = rv.ret;
            result.val = QUrl::fromUserInput(rv.val.toQString()).toLocalFile();
        }
    }

    return result;
}

void InteractiveProvider::closeQml(const QVariant& objectId)
{
    emit fireClose(objectId);
}

void InteractiveProvider::raiseQml(const QVariant& objectId)
{
    emit fireRaise(objectId);
}

void InteractiveProvider::onOpen(const QVariant& type, const QVariant& objectId, QObject* window)
{
    ContainerType::Type containerType = type.value<ContainerType::Type>();

    IF_ASSERT_FAILED(containerType != ContainerType::Undefined) {
        containerType = ContainerType::QmlDialog;
    }

    ObjectInfo objectInfo;
    objectInfo.uriQuery = m_openingUriQuery;
    objectInfo.objectId = objectId;
    objectInfo.window = window;
    if (!objectInfo.window) {
        objectInfo.window = (containerType == ContainerType::PrimaryPage) ? mainWindow()->qWindow() : qApp->focusWindow();
    }

    if (m_openingUriQuery.param("floating").toBool()) {
        m_openingUriQuery = UriQuery();
        m_floatingObjects.push_back(objectInfo);
        return;
    }

    if (ContainerType::PrimaryPage == containerType) {
        m_stack.clear();
        m_stack.push(objectInfo);
    } else if (ContainerType::QmlDialog == containerType) {
        m_stack.push(objectInfo);
    } else if (ContainerType::QWidgetDialog == containerType) {
        m_stack.push(objectInfo);
    } else {
        IF_ASSERT_FAILED_X(false, "unknown page type") {
            m_stack.push(objectInfo);
        }
    }

    notifyAboutCurrentUriChanged();
    m_opened.send(m_openingUriQuery.uri());

    m_openingUriQuery = UriQuery();
}

void InteractiveProvider::onClose(const QString& objectId, const QVariant& jsrv)
{
    m_retvals[objectId] = toRetVal(jsrv);

    bool found = false;
    for (int i = 0; i < m_stack.size(); ++i) {
        if (m_stack[i].objectId == objectId) {
            m_stack.remove(i);
            found = true;
            break;
        }
    }

    //! NOTE We may not find an object in the stack if it's,
    //! for example, a floating dialog (usually diagnostic dialogs)
    if (found) {
        notifyAboutCurrentUriChanged();
    } else {
        mu::remove_if(m_floatingObjects, [objectId](const ObjectInfo& obj) {
            return obj.objectId == objectId;
        });
    }

    m_fileDialogEventLoop.quit();
}

std::vector<InteractiveProvider::ObjectInfo> InteractiveProvider::allOpenObjects() const
{
    std::vector<ObjectInfo> result;

    result.insert(result.end(), m_stack.cbegin(), m_stack.cend());
    result.insert(result.end(), m_floatingObjects.cbegin(), m_floatingObjects.cend());

    return result;
}

void InteractiveProvider::notifyAboutCurrentUriChanged()
{
    m_currentUriChanged.send(currentUri().val);
}

void InteractiveProvider::notifyAboutCurrentUriWillBeChanged()
{
    m_currentUriAboutToBeChanged.notify();
}

// === QmlLaunchData ===
QmlLaunchData::QmlLaunchData(QObject* parent)
    : QObject(parent)
{
}

QVariant QmlLaunchData::value(const QString& key) const
{
    return m_data.value(key);
}

void QmlLaunchData::setValue(const QString& key, const QVariant& val)
{
    m_data[key] = val;
}

QVariant QmlLaunchData::data() const
{
    return m_data;
}
