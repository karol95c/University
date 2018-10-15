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
    std::cout << "Liczba obiektów: " << object.use_count() << "\n";
    std::cout << "object.readLines(): " << std::endl;
    object->readLines();
    std::cout << "Liczba obiektów: " << object.use_count() << "\n";

    std::cout << "object.readLines() dla drugiego wskaźnika: " << std::endl;
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
        std::cout << "Liczba obiektów: " << object.use_count() << "\n";
    }
    std::cout << "Liczba obiektów: " << object.use_count() << "\n";
    object.reset();
    if(!f1.is_open())
    {
        std::cout<< "Plik został zamknięty!\n";
    }
}
int main()
{
    std::cout << "Test liczby obiektów oraz funkcji ReadLines(): " << std::endl;
    testReadLines();
    std::cout << "Test zamykania pliku: " << std::endl;
    testClosingFile();
}