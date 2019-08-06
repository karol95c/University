#include <iostream>
#include <fstream>

class LineReader
{

    public:
    std::ifstream& file;
    LineReader(std::ifstream&);
    ~LineReader();
    
    void readLines();

};