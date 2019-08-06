#include "ArbiterRW.h"
#include <shared_mutex>
#include <chrono>
#include <condition_variable>

void ArbiterRW::makeReadDecision(int id)
{
    std::unique_lock<std::shared_timed_mutex> in(blockMutex);
    ++inCount;
    in.unlock();

    readFromFile(id);

    std::unique_lock<std::shared_timed_mutex> out(outMutex);
    ++outCount;

    if(wait && inCount == outCount)
    {
        writersMutex.unlock();
    }

    out.unlock();
}


void ArbiterRW::makeWriteDecision(int id,int value)
{
    std::unique_lock<std::shared_timed_mutex> in(blockMutex);
    std::unique_lock<std::shared_timed_mutex> out(outMutex);
    
    if (inCount == outCount)
    {
        out.unlock();
    }
    else
    {
        wait = true;
        out.unlock();
        writersMutex.unlock();
        wait = false;
    }
    
    writeToFile(id, value);

    in.unlock();
    
}

ArbiterRW::~ArbiterRW()
{}