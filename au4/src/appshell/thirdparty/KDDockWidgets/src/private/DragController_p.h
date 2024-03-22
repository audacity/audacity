/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_DRAGCONTROLLER_P_H
#define KD_DRAGCONTROLLER_P_H

#include "kddockwidgets/docks_export.h"

#include "TitleBar_p.h"
#include "WindowBeingDragged_p.h"

#include <QPoint>
#include <QMimeData>
#include <QTimer>

#include <memory>

namespace KDDockWidgets {

class StateBase;
class StateInternalMDIDragging;
class DropArea;
class Draggable;
class FallbackMouseGrabber;
class MinimalStateMachine;

using ResolveDropAreaFunc = std::function<DropArea*(const QPoint&)>;

class State : public QObject
{
    Q_OBJECT
public:
    explicit State(MinimalStateMachine *parent);
    ~State() override;

    template<typename Obj, typename Signal>
    void addTransition(Obj *, Signal, State *dest);
    bool isCurrentState() const;

    virtual void onEntry() = 0;
    virtual void onExit() {};

private:
    MinimalStateMachine *const m_machine;
};

class MinimalStateMachine : public QObject
{
    Q_OBJECT
public:
    explicit MinimalStateMachine(QObject *parent = nullptr);

    State *currentState() const;
    void setCurrentState(State *);

private:
    State *m_currentState = nullptr;
};

class DOCKS_EXPORT DragController : public MinimalStateMachine
{
    Q_OBJECT
    Q_PROPERTY(bool isDragging READ isDragging NOTIFY isDraggingChanged)
public:
    enum State
    {
        State_None = 0,
        State_PreDrag,
        State_Dragging
    };
    Q_ENUM(State)

    static DragController *instance();

    // Registers something that wants to be able to be dragged
    void registerDraggable(Draggable *);
    void unregisterDraggable(Draggable *);

    bool isDragging() const;
    bool isInNonClientDrag() const;
    bool isInClientDrag() const;

    void grabMouseFor(QWidgetOrQuick *);
    void releaseMouse(QWidgetOrQuick *);

    FloatingWindow *floatingWindowBeingDragged() const;

    ///@brief Returns the window being dragged
    WindowBeingDragged *windowBeingDragged() const;

    /// Experimental, internal, not for general use.
    void enableFallbackMouseGrabber();

    /**
     * @brief Allows the user to override the default algorithm for resolving DropArea
     * when a window is being dragged
     */
    void setResolveDropAreaFunc(ResolveDropAreaFunc func);

    ///@brief Used internally by the framework. Returns the function which was passed to setResolveDropAreaFunc()
    ///By default it's nullptr.
    ///@sa setResolveDropAreaFunc().
    ResolveDropAreaFunc resolveDropAreaFunc() const;

Q_SIGNALS:
    void mousePressed();
    void manhattanLengthMove();
    void manhattanLengthMoveMDI();
    void mdiPopOut();
    void dragCanceled();
    void dropped();
    void isDraggingChanged();

protected:
    bool eventFilter(QObject *, QEvent *) override;

private:
    friend class StateBase;
    friend class StateNone;
    friend class StatePreDrag;
    friend class StateDragging;
    friend class StateInternalMDIDragging;
    friend class StateDropped;
    friend class StateDraggingWayland;

    DragController(QObject * = nullptr);
    StateBase *activeState() const;
    WidgetType *qtTopLevelUnderCursor() const;
    DropArea *dropAreaUnderCursor() const;
    Draggable *draggableForQObject(QObject *o) const;
    QPoint m_pressPos;
    QPoint m_offset;

    Draggable::List m_draggables;
    Draggable *m_draggable = nullptr;
    QPointer<WidgetType> m_draggableGuard; // Just so we know if the draggable was destroyed for some reason
    std::unique_ptr<WindowBeingDragged> m_windowBeingDragged;
    DropArea *m_currentDropArea = nullptr;
    bool m_nonClientDrag = false;
    FallbackMouseGrabber *m_fallbackMouseGrabber = nullptr;
    StateInternalMDIDragging *m_stateDraggingMDI = nullptr;
    ResolveDropAreaFunc m_resolveDropAreaFunc = nullptr;
};

class StateBase : public State
{
    Q_OBJECT
public:
    explicit StateBase(DragController *parent);
    ~StateBase();

    // Not using QEvent here, to abstract platform differences regarding production of such events
    virtual bool handleMouseButtonPress(Draggable * /*receiver*/, QPoint /*globalPos*/, QPoint /*pos*/)
    {
        return false;
    }
    virtual bool handleMouseMove(QPoint /*globalPos*/)
    {
        return false;
    }
    virtual bool handleMouseButtonRelease(QPoint /*globalPos*/)
    {
        return false;
    }
    virtual bool handleMouseDoubleClick()
    {
        return false;
    }

    // Only interesting for Wayland
    virtual bool handleDragEnter(QDragEnterEvent *, DropArea *)
    {
        return false;
    }
    virtual bool handleDragLeave(DropArea *)
    {
        return false;
    }
    virtual bool handleDragMove(QDragMoveEvent *, DropArea *)
    {
        return false;
    }
    virtual bool handleDrop(QDropEvent *, DropArea *)
    {
        return false;
    }

    // Returns whether this is the current state
    bool isActiveState() const;

    DragController *const q;
};

class StateNone : public StateBase
{
    Q_OBJECT
public:
    explicit StateNone(DragController *parent);
    ~StateNone() override;
    void onEntry() override;
    bool handleMouseButtonPress(Draggable *draggable, QPoint globalPos, QPoint pos) override;
};

class StatePreDrag : public StateBase
{
    Q_OBJECT
public:
    explicit StatePreDrag(DragController *parent);
    ~StatePreDrag() override;
    void onEntry() override;
    bool handleMouseMove(QPoint globalPos) override;
    bool handleMouseButtonRelease(QPoint) override;
    bool handleMouseDoubleClick() override;
};

// Used on all platforms except Wayland. @see StateDraggingWayland
class StateDragging : public StateBase
{
    Q_OBJECT
public:
    explicit StateDragging(DragController *parent);
    ~StateDragging() override;
    void onEntry() override;
    void onExit() override;
    bool handleMouseButtonRelease(QPoint globalPos) override;
    bool handleMouseMove(QPoint globalPos) override;
    bool handleMouseDoubleClick() override;

private:
    QTimer m_maybeCancelDrag;
};


/// @brief State when we're moving an MDI dock widget around the main window
/// without it becoming floating
class StateInternalMDIDragging : public StateBase
{
    Q_OBJECT
public:
    explicit StateInternalMDIDragging(DragController *parent);
    ~StateInternalMDIDragging() override;
    void onEntry() override;
    bool handleMouseButtonRelease(QPoint globalPos) override;
    bool handleMouseMove(QPoint globalPos) override;
    bool handleMouseDoubleClick() override;
};

// Used on wayland only to use QDrag instead of setting geometry on mouse-move.
class StateDraggingWayland : public StateDragging
{
    Q_OBJECT
public:
    explicit StateDraggingWayland(DragController *parent);
    ~StateDraggingWayland() override;
    void onEntry() override;
    bool handleMouseButtonRelease(QPoint globalPos) override;
    bool handleDragEnter(QDragEnterEvent *, DropArea *) override;
    bool handleDragMove(QDragMoveEvent *, DropArea *) override;
    bool handleDragLeave(DropArea *) override;
    bool handleDrop(QDropEvent *, DropArea *) override;
    bool m_inQDrag = false;
};

// A sub-class just so we don't use QMimeData directly. We'll only accept drops if its mime data
// Can be qobject_casted to this class. For safety.
class WaylandMimeData : public QMimeData
{
    Q_OBJECT
public:
};

}

#endif
