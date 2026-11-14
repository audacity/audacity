/*
 * Audacity: A Digital Audio Editor
 */
#include "winleaktracker.h"

#include "framework/global/defer.h"

#if defined(_WIN32) && defined(_DEBUG)

#include <crtdbg.h>
#include <windows.h>
#include <dbghelp.h>

#include <algorithm>
#include <atomic>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <mutex>
#include <string>
#include <unordered_map>
#include <vector>

#pragma comment(lib, "dbghelp.lib")

namespace {
constexpr int kStackDepth = 32;
constexpr int kStackSkip = 2;   // skip allocHook + CRT allocator dispatch
constexpr int kGroupDepth = 5;  // group leaks by top N frames

// Mirrors the MSVC debug CRT block header (x64 layout). The user pointer
// returned by malloc_dbg sits immediately after this struct. Peeking it lets
// us recover the allocation serial on free/realloc without a second pointer
// -> serial map. Layout is an undocumented implementation detail but has been
// stable across MSVC 14.x.
struct CrtBlockHeader {
    CrtBlockHeader* next;
    CrtBlockHeader* prev;
    const char* fileName;
    int lineNumber;
    int blockUse;
    size_t dataSize;
    long requestNumber;
    unsigned char gap[4];
};

struct Stack {
    USHORT depth = 0;
    void* frames[kStackDepth] = {};
};

struct Record {
    size_t size = 0;
    Stack stack;
};

thread_local bool t_inHook = false;
std::atomic<bool> g_dumping{ false };
std::mutex g_mutex;
// Pointer, not value — keeps our own bookkeeping heap-allocated so we can
// null it out before dump without worrying about container teardown.
std::unordered_map<long, Record>* g_records = nullptr;

long serialFromPtr(void* userData)
{
    auto* hdr = reinterpret_cast<CrtBlockHeader*>(
        reinterpret_cast<char*>(userData) - sizeof(CrtBlockHeader));
    return hdr->requestNumber;
}

int __cdecl allocHook(int nAllocType, void* userData, size_t size, int nBlockUse,
                      long lRequest, const unsigned char* /*szFileName*/, int /*nLine*/)
{
    if (t_inHook || g_dumping.load(std::memory_order_relaxed)) {
        return TRUE;
    }
    if (nBlockUse == _CRT_BLOCK || nBlockUse == _IGNORE_BLOCK) {
        return TRUE;
    }

    t_inHook = true;
    {
        std::lock_guard<std::mutex> lock(g_mutex);
        if (!g_records) {
            g_records = new std::unordered_map<long, Record>();
            g_records->reserve(1 << 16);
        }

        switch (nAllocType) {
        case _HOOK_ALLOC: {
            Record r;
            r.size = size;
            r.stack.depth = CaptureStackBackTrace(kStackSkip, kStackDepth, r.stack.frames, nullptr);
            (*g_records)[lRequest] = r;
            break;
        }
        case _HOOK_REALLOC: {
            if (userData) {
                g_records->erase(serialFromPtr(userData));
            }
            Record r;
            r.size = size;
            r.stack.depth = CaptureStackBackTrace(kStackSkip, kStackDepth, r.stack.frames, nullptr);
            (*g_records)[lRequest] = r;
            break;
        }
        case _HOOK_FREE: {
            if (userData) {
                g_records->erase(serialFromPtr(userData));
            }
            break;
        }
        }
    }
    t_inHook = false;
    return TRUE;
}

struct SymbolInfo {
    std::string name;
    std::string file;
    DWORD line = 0;
};

SymbolInfo resolveSymbol(HANDLE process, DWORD64 addr)
{
    SymbolInfo info;
    alignas(SYMBOL_INFO) char buf[sizeof(SYMBOL_INFO) + 512] = {};
    auto* sym = reinterpret_cast<SYMBOL_INFO*>(buf);
    sym->SizeOfStruct = sizeof(SYMBOL_INFO);
    sym->MaxNameLen = 512;
    DWORD64 disp = 0;
    if (SymFromAddr(process, addr, &disp, sym)) {
        info.name = sym->Name;
    } else {
        char hex[32];
        _snprintf_s(hex, sizeof(hex), _TRUNCATE, "0x%llx", static_cast<unsigned long long>(addr));
        info.name = hex;
    }

    IMAGEHLP_LINE64 line = {};
    line.SizeOfStruct = sizeof(line);
    DWORD dispLine = 0;
    if (SymGetLineFromAddr64(process, addr, &dispLine, &line)) {
        info.file = line.FileName ? line.FileName : "";
        info.line = line.LineNumber;
    }
    return info;
}

void dumpReport(const char* path)
{
    // Stop all tracking first; after this, the hook is a no-op on every thread.
    g_dumping.store(true, std::memory_order_relaxed);
    t_inHook = true;

    std::unordered_map<long, Record>* records = nullptr;
    {
        std::lock_guard<std::mutex> lock(g_mutex);
        records = g_records;
        g_records = nullptr;
    }
    if (!records) {
        return;
    }

    muse::Defer freeRecords([records] { delete records; });
    if (records->empty()) {
        return;
    }

    HANDLE process = GetCurrentProcess();
    SymSetOptions(SYMOPT_LOAD_LINES | SYMOPT_UNDNAME | SYMOPT_DEFERRED_LOADS);
    SymInitialize(process, nullptr, TRUE);

    struct Group {
        size_t count = 0;
        size_t totalBytes = 0;
        Stack example;
    };
    std::map<std::vector<void*>, Group> groups;

    for (auto& [serial, rec] : *records) {
        std::vector<void*> key;
        const int d = std::min<int>(rec.stack.depth, kGroupDepth);
        key.reserve(d);
        for (int i = 0; i < d; ++i) {
            key.push_back(rec.stack.frames[i]);
        }
        auto& g = groups[key];
        if (g.count == 0) {
            g.example = rec.stack;
        }
        g.count++;
        g.totalBytes += rec.size;
    }

    std::vector<std::pair<const std::vector<void*>*, const Group*> > ranked;
    ranked.reserve(groups.size());
    for (auto& [k, v] : groups) {
        ranked.push_back({ &k, &v });
    }
    std::sort(ranked.begin(), ranked.end(), [](auto& a, auto& b) {
        return a.second->totalBytes > b.second->totalBytes;
    });

    FILE* f = nullptr;
    if (fopen_s(&f, path, "w") != 0 || !f) {
        SymCleanup(process);
        return;
    }

    size_t totalCount = 0, totalBytes = 0;
    for (auto& [k, v] : groups) {
        totalCount += v.count;
        totalBytes += v.totalBytes;
    }

    std::fprintf(f, "Audacity CRT leak report (aggregated by top %d call-stack frames)\n", kGroupDepth);
    std::fprintf(f, "Total leaked blocks: %zu\n", totalCount);
    std::fprintf(f, "Total leaked bytes:  %zu\n", totalBytes);
    std::fprintf(f, "Unique call sites:   %zu\n\n", groups.size());

    int rank = 0;
    for (auto& [keyPtr, gPtr] : ranked) {
        rank++;
        std::fprintf(f, "#%d  count=%zu  total=%zu bytes\n", rank, gPtr->count, gPtr->totalBytes);
        const int d = std::min<int>(gPtr->example.depth, kStackDepth);
        for (int i = 0; i < d; ++i) {
            const DWORD64 addr = reinterpret_cast<DWORD64>(gPtr->example.frames[i]);
            SymbolInfo s = resolveSymbol(process, addr);
            if (!s.file.empty()) {
                std::fprintf(f, "    %s  (%s:%lu)\n", s.name.c_str(), s.file.c_str(),
                             static_cast<unsigned long>(s.line));
            } else {
                std::fprintf(f, "    %s\n", s.name.c_str());
            }
        }
        std::fputc('\n', f);
    }

    std::fclose(f);
    SymCleanup(process);
}
} // namespace

namespace au::app::winleaktracker {
void install()
{
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);

    // Build is /SUBSYSTEM:WINDOWS so stderr is unattached; redirect it to a
    // file and route the CRT report there. The default sink is
    // OutputDebugString, which is synchronous and adds ~10 s to shutdown for
    // a long report.
    FILE* crtLog = nullptr;
    if (freopen_s(&crtLog, "audacity_crt_leaks.log", "w", stderr) == 0) {
        _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
        _CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDERR);
        _CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE);
        _CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDERR);
    }

    _CrtSetAllocHook(allocHook);
    std::atexit([] { dumpReport("audacity_leak_stacks.log"); });
}
} // namespace au::app::winleaktracker

#endif
