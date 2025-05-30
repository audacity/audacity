// ParallelUtils.h
#pragma once
#include <thread>
#include <vector>
#include <functional>

class ParallelExecutor {
public:
    static void ExecuteInParallel(int numThreads, std::function<void(int)> task) {
        if (numThreads <= 0) return;

        std::vector<std::thread> threads;
        for (int i = 0; i < numThreads; ++i) {
            threads.emplace_back([i, &task]() {
                task(i);
            });
        }
        for (auto& t : threads) {
            t.join();
        }
    }
};
