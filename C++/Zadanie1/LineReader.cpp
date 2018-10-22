#include "LineReader.h"

#include <iostream>
#include <fstream>
#include <memory>
#include <utility>

LineReader::LineReader(std::ifstream& f) : file(f)
{
    std::cout << "Creating object." << std::endl;
}
LineReader::~LineReader()
{
    file.close();
    if (!file.is_open())
    {
        std::cout << "Deleting object, file has been closed." << std::endl;
    }
}
    
void LineReader::readLines()
{
    file.clear();
    file.seekg(0, std::ios::beg);

    file.exceptions(std::ifstream::eofbit);
    try
    {
        std::string output;

        while (getline(file, output))
        {
            std::cout << output + "\n";
        }
    }
    catch (std::ifstream::failure e)
    {
       std::cerr << "EXCEPTION! END_OF_FILE\n";
    }
}


void testReadLines()
{
    std::ifstream f1("test.txt");
    
    auto object = std::make_shared<LineReader>(f1);
    auto object1 = object;
    auto object2 = object;
    auto object3 = object;
    auto object4 = object;
    std::cout << "Objects count: " << object.use_count() << "\n";
    std::cout << "object.readLines(): " << std::endl;
    object->readLines();
    std::cout << "Objects count: " << object.use_count() << "\n";

    std::cout << "object.readLines() for second shared_ptr: " << std::endl;
    object1->readLines();
}

void testClosingFile()
{
    std::ifstream f1("test.txt");
    auto object = std::make_shared<LineReader>(f1);
    {
        auto object1 = object;
        auto object2 = object;
        auto object3 = object;
        auto object4 = object;
        std::cout << "Objects count: " << object.use_count() << "\n";
    }
    std::cout << "Objects count: " << object.use_count() << "\n";
    object.reset();
    if(!f1.is_open())
    {
        std::cout<< "File has been closed\n";
    }
}
int main()
{
    std::cout << "Objects counter and ReadLines test: " << std::endl;
    testReadLines();
    std::cout << "Closing file test: " << std::endl;
    testClosingFile();
}