#include <iostream>
#include <fstream>


int main()
{
    std::fstream inputFile;
    std::string inputFileName = "test.txt";
    
    inputFile.open(inputFileName, std::ios::in);
    
    if (inputFile.is_open())
    {
        std::string line;
        while(inputFile)
        {
            std::getline(inputFile, line);
            std::cout <<line << std::endl;

        }
        inputFile.close();
    }
    else
    {
        std::cout << "Cannot open file: " << inputFileName << std::endl;
    }


    return 0;
}