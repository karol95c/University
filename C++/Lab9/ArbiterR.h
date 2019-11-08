#ifndef ARBITERR_H
#define ARBITERR_H

#include "Arbiter.h"
#include <shared_mutex>
#include <chrono>
#include <condition_variable>

class ArbiterR : public Arbiter
{
    int readersCount;
    std::shared_timed_mutex controlMutex;

    public:
    ~ArbiterR();
    ArbiterR(std::string fileName)
    {
        fileStream.open (fileName, std::fstream::in | std::fstream::out | std::fstream::app);
    }

    void makeReadDecision(int) override;
    void makeWriteDecision(int, int) override;
};

#endif /*ARBITERR_H*/