#include <iostream>
#include <fstream>


void reader()
{

    std::fstream fileStream;
    fileStream.open ("test.txt", std::fstream::in | std::fstream::out | std::fstream::app);
    std::string lastLine;
    if(fileStream.is_open())
    {
        fileStream.seekg(-1, std::ios::end);

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

    std::cout << "Reader " << " read: " << lastLine;
    }
}

int main()
{
    reader();
    return 0;
}