/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */
/*
    QM DSP Library

    Centre for Digital Music, Queen Mary, University of London.
    This file Copyright 2009 QMUL.
*/

#ifndef QM_DSP_ASYNCHRONOUS_TASK_H
#define QM_DSP_ASYNCHRONOUS_TASK_H

#include "Thread.h"

#include <iostream>

/**
 * AsynchronousTask provides a thread pattern implementation for
 * threads which are used to perform a series of similar operations in
 * parallel with other threads of the same type.
 *
 * For example, a thread used to calculate FFTs of a particular block
 * size in the context of a class that needs to calculate many block
 * sizes of FFT at once may be a candidate for an AsynchronousTask.
 *
 * The general use pattern is:
 *
 *   caller -> request thread A calculate something
 *   caller -> request thread B calculate something
 *   caller -> request thread C calculate something
 *   caller -> wait for threads A, B, and C
 *
 * Here threads A, B, and C may be AsynchronousTasks.  An important
 * point is that the caller must be prepared to block when waiting for
 * these threads to complete (i.e. they are started asynchronously,
 * but testing for completion is synchronous).
 */
class AsynchronousTask : public Thread
{
public:
    AsynchronousTask() :
        m_todo("AsynchronousTask: task to perform"),
        m_done("AsynchronousTask: task complete"),
        m_inTask(false),
        m_finishing(false) {
        start();
    }
    
    virtual ~AsynchronousTask() {
        m_todo.lock();
        m_finishing = true;
        m_todo.signal();
        m_todo.unlock();
        wait();
    }

    // Subclass must provide methods to request task and obtain
    // results, which the caller calls.  The method that requests a
    // new task should set up any internal state and call startTask(),
    // which then calls back on the subclass implementation of
    // performTask from within its work thread.  The method that
    // obtains results should call awaitTask() and then return any
    // results from internal state.

protected:
    void startTask() {
        m_done.lock();
        m_todo.lock();
        m_inTask = true;
        m_todo.signal();
        m_todo.unlock();
    }
    void awaitTask() {
        m_done.wait();
        m_done.unlock();
    }

    virtual void performTask() = 0;
    
private:
    virtual void run() {
        m_todo.lock();
        while (1) {
            while (!m_inTask && !m_finishing) {
                m_todo.wait();
            }
            if (m_finishing) {
                m_done.lock();
                m_inTask = false;
                m_done.signal();
                m_done.unlock();
                break;
            }
            performTask();
            m_done.lock();
            m_inTask = false;
            m_done.signal();
            m_done.unlock();
        }
        m_todo.unlock();
    }

    Condition m_todo;
    Condition m_done;
    bool m_inTask;
    bool m_finishing;
};

#endif
