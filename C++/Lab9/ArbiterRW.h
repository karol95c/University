#ifndef ARBITERRW_H_
#define ARBITERRW_H_

#include "Arbiter.h"
#include <shared_mutex>
#include <chrono>
#include <condition_variable>

class ArbiterRW : public Arbiter
{
    int inCount;
    int outCount;
    bool wait;
    std::shared_timed_mutex controlMutex;
    std::shared_timed_mutex outMutex;

    public:
    ~ArbiterRW();
    ArbiterRW(std::string fileName)
    {
        wait = false;
        fileStream.open (fileName, std::fstream::in | std::fstream::out | std::fstream::app);
        writersMutex.lock();
    }

    void makeReadDecision(int) override;
    void makeWriteDecision(int, int) override;
};

#endif /*ARBITERRW_H_*/