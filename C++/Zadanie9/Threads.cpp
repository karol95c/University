#include "Arbiter.h"
#include "ArbiterR.h"
#include "ArbiterW.h"
#include "ArbiterRW.h"

#include <iostream>
#include <functional>
#include <thread>
#include <vector>
#include <mutex>
#include <random>
#include <fstream>




class Writer
{
    int id;
    std::random_device rd;
    std::mt19937_64 generator;
    std::uniform_int_distribution<> normalDist;
    Arbiter* p_arb;


    public:
    Writer()
    {
        p_arb = nullptr;
    }
    Writer(Arbiter* a) : generator(std::mt19937_64(rd())), normalDist(0,1000)
    {
        p_arb = a;
        id = p_arb->assignID();
        std::uniform_int_distribution<> normalDist(0, 1000);
        if(p_arb == nullptr)
        {
            std::cout << "KKRZYCZ" << std::endl;
        }
    }
    Writer(const Writer& w)
    {
        id = w.id;
        generator = w.generator;
        normalDist = w.normalDist;
        p_arb = w.p_arb;
        if(p_arb == nullptr)
        {
            std::cout << "KopiaRZYCZ" << std::endl;
        }
    }
    ~Writer()
    {
    }

    void write()
    {
        int rnd = normalDist(generator);
        if(p_arb == nullptr)
        {
            std::cout << "KRZYCZ" << std::endl;
        }
        p_arb->makeWriteDecision(id, rnd);
    }
};

class Reader{
    int id;
    Arbiter* p_arb;

    
    public:
    Reader() = default;
    Reader(Arbiter* a) : p_arb(a)
    {
       id = a->assignID();
    }

    void read()
    {
        p_arb->makeReadDecision(id);

    }
    ~Reader()
    {
    }
};

void getWritersVector(Arbiter* arb, std::vector<Writer*>& cont, int size)
{
    for(int i = 0; i < size; ++i)
    {
        cont[i] = new Writer(arb);
    }
}

void getReadersVector(Arbiter* arb, std::vector<Reader*>& cont, int size)
{
    for(int i = 0; i < size; ++i)
    {
        cont[i] = new Reader(arb);
    }
}

void readersPreference()
{
    Arbiter* arb = new ArbiterR("test1.txt");
    std::vector<Writer*> writersVec(50, nullptr);
    std::vector<Reader*> readersVec(50, nullptr);

    getWritersVector(arb, writersVec, 50);
    getReadersVector(arb, readersVec, 50);


    for(int i = 0; i < 10; ++i)
    {
        std::thread f2(&Reader::read, readersVec[i]);
        std::thread f(&Writer::write, writersVec[i]);
        f.join();
        f2.join();
    }

    for(int i = 0; i < 50; ++i)
    {
        delete writersVec[i];
        delete readersVec[i];
    }

    delete arb;   
}

void writersPreference()
{
    Arbiter* arb = new ArbiterW("test2.txt");
    std::vector<Writer*> writersVec(50, nullptr);
    std::vector<Reader*> readersVec(50, nullptr);

    getWritersVector(arb, writersVec, 50);
    getReadersVector(arb, readersVec, 50);


    for(int i = 0; i < 10; ++i)
    {
        std::thread f2(&Reader::read, readersVec[i]);
        std::thread f(&Writer::write, writersVec[i]);
        f.join();
        f2.join();
    }

    for(int i = 0; i < 50; ++i)
    {
        delete writersVec[i];
        delete readersVec[i];
    }

    delete arb; 
}

void equalPreference()
{
    Arbiter* arb = new ArbiterRW("test3.txt");
    std::vector<Writer*> writersVec(50, nullptr);
    std::vector<Reader*> readersVec(50, nullptr);

    getWritersVector(arb, writersVec, 50);
    getReadersVector(arb, readersVec, 50);


    for(int i = 0; i < 10; ++i)
    {
        std::thread f2(&Reader::read, readersVec[i]);
        std::thread f(&Writer::write, writersVec[i]);
        f.join();
        f2.join();
    }

    // for(int i = 0; i < 50; ++i)
    // {
    //     delete writersVec[i];
    //     delete readersVec[i];
    // }

    // delete arb; 
}


int main ()
{
    std::cout << "----------------------------------------------------------------" << std::endl;
    std::cout << "Readers preference:" << std::endl;
    std::thread r(readersPreference);
    r.join();
    std::cout << "----------------------------------------------------------------" << std::endl;
    std::cout << "Writers preference" << std::endl;
    std::thread w(writersPreference);
    w.join();
    std::cout << "----------------------------------------------------------------" << std::endl;
    std::cout << "Equal preference" << std::endl;
    std::thread e(equalPreference);
    e.join();
    std::cout << "----------------------------------------------------------------" << std::endl;
    return 0;
}