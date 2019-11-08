#include <iostream>
#include <fstream>

int main()
{
    std::fstream outputFile;

    std::string outputFileName = "test.txt";

    outputFile.open(outputFileName, std::ios::out);

    if (outputFile.is_open())
    {
        outputFile << "First line.\n";
        outputFile << "Second line.\n";
        outputFile << "Third line.\n";
        outputFile.close();

    }
    else
    {
        std::cout << "Cannot read file: " << outputFileName <<std::endl;
    }
    
    return 0;

}