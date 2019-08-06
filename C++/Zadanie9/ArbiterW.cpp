#include "ArbiterW.h"
#include <shared_mutex>
#include <chrono>
#include <condition_variable>

void ArbiterW::makeReadDecision(int id)
{
    std::shared_lock<std::shared_timed_mutex> r (readersMutex, std::defer_lock);
    if(r.try_lock_for(std::chrono::microseconds(100)))
    {
        readFromFile(id);
    }
    

}


void ArbiterW::makeWriteDecision(int id,int value)
{
    ++writersCount;
    std::unique_lock<std::shared_timed_mutex> w(writersMutex);
    readersMutex.lock();
    writeToFile(id, value);
    --writersCount;
    if (writersCount == 0)
    {
        readersMutex.unlock();
    }
}

ArbiterW::~ArbiterW()
{}