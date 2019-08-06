#include "ArbiterR.h"
#include <shared_mutex>
#include <chrono>
#include <condition_variable>

void ArbiterR::makeReadDecision(int id)
{
    std::shared_lock<std::shared_timed_mutex> r(blockMutex);
    std::unique_lock<std::shared_timed_mutex> c(controlMutex);
    // std::lock(r, c);

    ++readersCount;

    c.unlock();

    readFromFile(id);

    c.lock();

    --readersCount;
    c.unlock();
    r.unlock();

}


void ArbiterR::makeWriteDecision(int id,int value)
{
    std::unique_lock<std::shared_timed_mutex> r(blockMutex, std::defer_lock);
    if(r.try_lock_for(std::chrono::microseconds(100)))
    {
        writeToFile(id, value);
    }
}

ArbiterR::~ArbiterR()
{}