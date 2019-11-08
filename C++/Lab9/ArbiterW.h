#ifndef ARBITERW_H_
#define ARBITERW_H_

#include "Arbiter.h"
#include <shared_mutex>
#include <chrono>
#include <condition_variable>

class ArbiterW : public Arbiter
{
    int readersCount;
    int writersCount;
    std::shared_timed_mutex readersMutex;
    std::shared_timed_mutex writersMutex;

    public:
    ~ArbiterW();
    ArbiterW(std::string fileName)
    {
        fileStream.open (fileName, std::fstream::in | std::fstream::out | std::fstream::app);
    }

    void makeReadDecision(int) override;
    void makeWriteDecision(int, int) override;
};

#endif /*ARBITERW_H_*/