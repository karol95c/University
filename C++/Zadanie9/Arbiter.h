#ifndef ARBITER_H_
#define ARBITER_H_

#include <fstream>
#include <thread>
#include <mutex>
#include <shared_mutex>

class Arbiter
{
    protected:

    int ids = 10;
    std::fstream fileStream;
    std::shared_timed_mutex blockMutex;
    std::shared_timed_mutex writersMutex;

    public:

    Arbiter(std::string fileName);
    Arbiter() = default;
    virtual ~Arbiter();
    int assignID();
    virtual void readFromFile(int);
    virtual void writeToFile(int, int);
    std::fstream* getFStreamPtr();
    virtual void makeWriteDecision(int id, int value)= 0;
    virtual void makeReadDecision(int id){};
};

#endif /*ARBITER_H_*/