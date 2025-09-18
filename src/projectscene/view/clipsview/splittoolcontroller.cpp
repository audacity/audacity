/*
* Audacity: A Digital Audio Editor
*/

#include "splittoolcontroller.h"

#include <cmath>

#include "internal/tapholdshortcut.h"
#include "log.h"

namespace au::projectscene {
SplitToolController::SplitToolController(QObject* parent)
    : QObject(parent)
{
    QPixmap pixmap(":/images/customCursorShapes/Split.png");
    m_cursor = QCursor(pixmap.scaled(32, 32, Qt::KeepAspectRatio, Qt::SmoothTransformation));

    dispatcher()->reg(this, "split-tool", [this]() {
        setActive(!active());
    });

    m_shortcut = std::make_unique<TapHoldShortcut>("split-tool");

    m_shortcut->pressed().onNotify(nullptr, [this]() {
        m_prePressState = active();
        if (active()) {
            return;
        }
        setActive(true);
    });

    m_shortcut->tapped().onNotify(nullptr, [this]() {
        setActive(!m_prePressState);
    });

    m_shortcut->holdStarted().onNotify(nullptr, [this]() {
        setActive(true);
    });

    m_shortcut->holdEnded().onNotify(nullptr, [this]() {
        setActive(false);
    });

    qApp->installEventFilter(this);
}

SplitToolController::~SplitToolController() = default;

void au::projectscene::SplitToolController::doSplit()
{
    Qt::KeyboardModifiers mods = QGuiApplication::queryKeyboardModifiers();

    if (mods & Qt::ShiftModifier) {
        auto allTracks = globalContext()->currentProject()->trackeditProject()->trackIdList();
        splitTracksAt(allTracks, context()->positionToTime(m_guidelinePos));
    } else if (m_hoveredTrack >= 0) {
        splitTracksAt({ m_hoveredTrack }, context()->positionToTime(m_guidelinePos));
    }
}

void SplitToolController::mouseDown(double pos)
{
    if (!active()) {
        return;
    }
    updateGuideline(pos);
    m_splitStartPos = m_guidelinePos;
    doSplit();
}

void SplitToolController::mouseMove(double pos)
{
    if (!active()) {
        return;
    }
    updateGuideline(pos);
}

void SplitToolController::mouseUp(double pos)
{
    if (!active()) {
        return;
    }

    updateGuideline(pos);

    if (std::abs(m_splitStartPos - m_guidelinePos) < MIN_DOUBLE_SPLIT_DISTANCE) {
        return;
    }

    doSplit();
}

double SplitToolController::guidelinePosition() const
{
    return m_guidelinePos;
}

bool SplitToolController::guidelineVisible() const
{
    return m_active && m_clipHovered && muse::RealIsEqualOrMore(m_guidelinePos, 0);
}

TimelineContext* SplitToolController::context() const
{
    return m_context;
}

void SplitToolController::setContext(TimelineContext* newContext)
{
    m_context = newContext;
    emit contextChanged();
}

bool SplitToolController::active() const
{
    return m_active;
}

void SplitToolController::setActive(bool newActive)
{
    if (m_active == newActive) {
        return;
    }
    m_active = newActive;

    auto prj = globalContext()->currentProject();
    if (prj) {
        prj->viewState()->setSplitToolEnabled(m_active);
    }

    updateCursor();

    emit activeChanged();
    emit guidelineVisibleChanged();
}

void SplitToolController::setGuidelinePos(double newGuidelinePos)
{
    if (muse::is_equal(m_guidelinePos, newGuidelinePos)) {
        return;
    }

    m_guidelinePos = newGuidelinePos;
    emit guidelinePositionChanged();
    emit guidelineVisibleChanged();
}

void SplitToolController::updateGuideline(double pos)
{
    const bool snapEnabled = true;
    double guideline = context()->findGuideline(context()->positionToTime(pos, snapEnabled));

    if (!muse::RealIsEqualOrMore(guideline, 0)) {
        guideline = context()->positionToTime(pos);
    }

    const double newPos = context()->timeToPosition(guideline);
    if (muse::is_equal(newPos, m_guidelinePos)) {
        return;
    }
    const bool wasVisible = guidelineVisible();

    m_guidelinePos = newPos;
    emit guidelinePositionChanged();

    if (wasVisible != guidelineVisible()) {
        emit guidelineVisibleChanged();
    }
}

void SplitToolController::updateCursor()
{
    if (active() && clipHovered()) {
        overrideCursor();
    } else {
        restoreCursor();
    }
}

void SplitToolController::overrideCursor()
{
    if (!m_cursorOverriden) {
        m_cursorOverriden = true;
        QGuiApplication::setOverrideCursor(m_cursor);
    }
}

void SplitToolController::restoreCursor()
{
    if (m_cursorOverriden) {
        m_cursorOverriden = false;
        QGuiApplication::restoreOverrideCursor();
    }
}

void SplitToolController::splitTrackAt(trackedit::TrackId id, double t)
{
    if (id < 0) {
        return;
    }
    std::vector<muse::secs_t> pivots { t };

    LOGD() << "Splitting track at " << t;
    dispatcher()->dispatch("track-split-at",
                           muse::actions::ActionData::make_arg2<trackedit::TrackIdList, std::vector<muse::secs_t> >({ id },
                                                                                                                    pivots));
}

void SplitToolController::splitTracksAt(trackedit::TrackIdList ids, double t)
{
    if (ids.empty()) {
        return;
    }
    std::vector<muse::secs_t> pivots { t };

    LOGD() << "Splitting tracks at " << t;
    dispatcher()->dispatch("track-split-at",
                           muse::actions::ActionData::make_arg2<trackedit::TrackIdList, std::vector<muse::secs_t> >(ids, pivots));
}

bool SplitToolController::clipHovered() const
{
    return m_clipHovered;
}

void SplitToolController::setClipHovered(bool newClipHovered)
{
    if (m_clipHovered == newClipHovered) {
        return;
    }

    m_clipHovered = newClipHovered;

    updateCursor();

    emit clipHoveredChanged();
    emit guidelineVisibleChanged();
}

int SplitToolController::hoveredTrack() const
{
    return m_hoveredTrack;
}

void SplitToolController::setHoveredTrack(int newHoveredTrack)
{
    if (m_hoveredTrack == newHoveredTrack) {
        return;
    }
    m_hoveredTrack = newHoveredTrack;
    emit hoveredTrackChanged();
}

bool SplitToolController::eventFilter(QObject* obj, QEvent* event)
{
    if (event->type() == QEvent::ShortcutOverride) {
        auto* ke = static_cast<QKeyEvent*>(event);
        if (ke->key() == Qt::Key_Escape && active()) {
            setActive(false);
            event->accept();
            return true;
        }
    }
    return QObject::eventFilter(obj, event);
}
}
