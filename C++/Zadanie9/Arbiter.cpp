#include "Arbiter.h"

#include <fstream>
#include <iostream>


Arbiter::Arbiter(std::string fileName)
{
    fileStream.open (fileName, std::fstream::in | std::fstream::out | std::fstream::app);
}


Arbiter::~Arbiter()
{
    fileStream.close();
}

int Arbiter::assignID()
{
    return ids++;
}

void Arbiter::readFromFile(int id)
{
    std::string lastLine;
    

    if(fileStream.is_open())
    {
        fileStream.seekg(-2, std::ios::end);

        bool keepLooping = true;
        while(keepLooping)
        {
            char ch;
            fileStream.get(ch);

            if(static_cast<int>(fileStream.tellg()) <= 1)
            {
                fileStream.seekg(0);
                lastLine = "File is empty.";
                keepLooping = false;
            }
            else if(ch == '\n')
            {
                keepLooping = false;
            }
            else
            {
                fileStream.seekg(-2, std::ios_base::cur);
            }
        }
        getline(fileStream, lastLine);
        lastLine.erase(0, 3);

    std::cout << "Reader " << id << " read: " << lastLine << std::endl;
    }
}

void Arbiter::writeToFile(int id, int value)
{
    fileStream << id << ": " << value << std::endl;
}

std::fstream* Arbiter::getFStreamPtr()
{
    return &fileStream;
}



