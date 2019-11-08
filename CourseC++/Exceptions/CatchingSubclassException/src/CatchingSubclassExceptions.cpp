#include <iostream>
#include <exception>

void goesWrong()
{
    bool error1 = true;
    bool error2 = false;

    if(error1)
    {
        throw std::bad_alloc();
    }

    if(error2)
    {
        throw std::exception();
    }
}

int main()
{
    try
    {
        goesWrong();
    }
    catch (std::bad_alloc &e)
    {
        std::cout << "Catch bad_alloc: " << e.what() << std::endl;
    }
    catch (std::exception &e)
    {
        std::cout << "Catch exception: " << e.what() << std::endl;
    }

    std::cout << "Still working\n";
    return 0;
}