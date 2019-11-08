#include <iostream>
#include <fstream>


int main()
{
    std::ifstream inputFile;
    std::string inputFileName = "population.txt";
    
    inputFile.open(inputFileName);
    
    if (!inputFile.is_open())
    {
        return 1;
    }

    std::string line;
    int population;

    while (inputFile)
    {
        getline(inputFile, line, ':');
        inputFile >> population;

        inputFile >> std::ws;

        if (!inputFile)
        {
            break;
        }

        std::cout << "'" << line << "'" <<  "'" << " --- " <<  "'" <<  "'" << population  <<  "'\n";
    }
    



    return 0;
}