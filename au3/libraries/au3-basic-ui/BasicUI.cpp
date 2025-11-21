/*!********************************************************************

Audacity: A Digital Audio Editor

@file BasicUI.cpp

Paul Licameli

**********************************************************************/
#include "BasicUI.h"

#include <mutex>
#include <vector>

#if (defined(__linux__) && !defined(__ANDROID__)) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define HAS_XDG_OPEN_HELPER
#endif

#if defined(HAS_XDG_OPEN_HELPER)

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <unistd.h>
#include <fcntl.h>
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

#include <string>

#if defined(__FreeBSD__) || defined(__OpenBSD__)
extern char** environ;
#endif

namespace {
const char* ENV_PREFIX = "APPIMAGE_PRESERVED_";

bool IsPreservedEnvVar(const char* var)
{
    return strncmp(var, ENV_PREFIX, strlen(ENV_PREFIX)) == 0;
}

bool ResetEnv()
{
    for (auto envIterator = environ; *envIterator; ++envIterator) {
        auto envVar = *envIterator;

        if (!IsPreservedEnvVar(envVar)) {
            continue;
        }

        auto separator = strchr(envVar, '=');

        // Why?
        if (!separator) {
            continue;
        }

        auto varName = std::string(envVar + strlen(ENV_PREFIX), separator);

        if (varName.empty()) {
            continue;
        }

        auto varValue = separator + 1;

        const auto result = *varValue
                            ? setenv(varName.c_str(), varValue, true)
                            : unsetenv(varName.c_str());

        if (result != 0) {
            return false;
        }
    }

    return true;
}

std::string FindXDGOpen()
{
    const char* path = getenv("PATH");

    if (!path) {
        return {};
    }

    std::string result;

    while (*path) {
        const char* colon = strchr(path, ':');
        if (!colon) {
            colon = path + strlen(path);
        }

        result.assign(path, colon);
        result += "/xdg-open";

        if (access(result.c_str(), X_OK) == 0) {
            return result;
        }

        path = colon;

        if (*path == ':') {
            ++path;
        }
    }

    return {};
}

bool RunXDGOpen(const std::string& uri)
{
    std::string xdgOpen = FindXDGOpen();

    if (xdgOpen.empty()) {
        return false;
    }

    pid_t pid = fork();

    if (pid == -1) {
        return false;
    }

    if (pid == 0) {
        // Fork again. This way OS is now responsible for cleaning up the
        // (grand)child process.
        int secondFork = fork();

        if (secondFork < 0) {
            return false;
        }

        if (secondFork == 0) {// Close all file descriptors except stdin, stdout, stderr
            struct rlimit rlim;
            if (getrlimit(RLIMIT_NOFILE, &rlim) == 0) {
                for (int fd = STDERR_FILENO + 1; fd < rlim.rlim_cur; ++fd) {
                    close(fd);
                }
            }

            // Open /dev/null as stdin, stdout, stderr
            int fd = open("/dev/null", O_RDWR);

            if (fd != -1) {
                dup2(fd, STDIN_FILENO);
                dup2(fd, STDOUT_FILENO);
                dup2(fd, STDERR_FILENO);

                if (fd > STDERR_FILENO) {
                    close(fd);
                }
            }

            if (!ResetEnv()) {
                _exit(1);
            }

            char* const args[] = {
                const_cast<char*>(xdgOpen.c_str()),
                const_cast<char*>(uri.c_str()),
                nullptr
            };
            // Run xdg-open
            execv(xdgOpen.c_str(), args);

            // If we get here, something went wrong
            _exit(1);
        } else {
            // Grandchild has started, so we can exit
            _exit(0);
        }
    } else {
        int status;
        waitpid(pid, &status, 0);

        return WIFEXITED(status) && WEXITSTATUS(status) == 0;
    }

    return false;
}
}

#endif

namespace  BasicUI {
WindowPlacement::~WindowPlacement() = default;

WindowPlacement::operator bool() const {
    return false;
}

Services::~Services() = default;

ProgressDialog::~ProgressDialog() = default;

GenericProgressDialog::~GenericProgressDialog() = default;

static Services* theInstance = nullptr;

Services* Get() { return theInstance; }

Services* Install(Services* pInstance)
{
    auto result = theInstance;
    theInstance = pInstance;
    return result;
}

static std::recursive_mutex sActionsMutex;
static std::vector<Action> sActions;

void CallAfter(Action action)
{
    if (auto p = Get()) {
        p->DoCallAfter(action);
    } else {
        // No services yet -- but don't lose the action.  Put it in a queue
        auto guard = std::lock_guard{ sActionsMutex };
        sActions.emplace_back(std::move(action));
    }
}

void Yield()
{
    do {
        // Dispatch anything in the queue, added while there were no Services
        {
            auto guard = std::lock_guard{ sActionsMutex };
            std::vector<Action> actions;
            actions.swap(sActions);
            for (auto& action : actions) {
                action();
            }
        }

        // Dispatch according to Services, if present
        if (auto p = Get()) {
            p->DoYield();
        }
    }
    // Re-test for more actions that might have been enqueued by actions just
    // dispatched
    while (!sActions.empty());
}

bool OpenInDefaultBrowser(const wxString& url)
{
#if defined(HAS_XDG_OPEN_HELPER)
    if (RunXDGOpen(url.ToStdString())) {
        return true;
    }
#endif

    if (auto p = Get()) {
        return p->DoOpenInDefaultBrowser(url);
    }

    return false;
}

TranslatableString DefaultCaption()
{
    return XO("Message");
}
}
