#include <iostream>


class CanGoWrong{
    public:
    CanGoWrong()
    {
        char *memory = new char[99999999999];
        delete[] memory;
    }
};

int main()
{
    try
    {
        CanGoWrong a;
    }
    catch (std::bad_alloc &e)
    {
        std::cout << "Error: " << e.what() << std::endl;
    }


    std::cout << "Still working";
    return 0;
}