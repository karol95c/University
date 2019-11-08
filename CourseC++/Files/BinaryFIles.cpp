#include <iostream>
#include <fstream>

#pragma pack(push, 1)

struct Person
{
    char name[50];
    int age;
    double height;
};

#pragma pack(pop)

int main()
{
    Person grandpa = {"Joshua", 86, 176.2};
    std::string fileName = "test.bin";
    
    std::ofstream outputFile;
    outputFile.open(fileName, std::ios::binary);
    if(outputFile.is_open())
    {
        outputFile.write(reinterpret_cast<char*>(&grandpa), sizeof(Person));
        outputFile.close();
    }
    else
    {
        std::cout << "Cannot open file: " << fileName << std::endl;
    }

    Person someone = {};
    std::ifstream inputFile;
    inputFile.open(fileName, std::ios::binary);
    if(inputFile.is_open())
    {
        inputFile.read(reinterpret_cast<char*>(&someone), sizeof(Person));
        inputFile.close();
    }
    else
    {
        std::cout << "Cannot open file: " << fileName << std::endl;
    }

    std::cout << "name: " << someone.name << "\nage: " << someone.age << "\nheight: " << someone.height << std::endl;



    return 0;
}