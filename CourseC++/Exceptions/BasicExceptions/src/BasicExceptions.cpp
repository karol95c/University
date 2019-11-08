#include <iostream>


void mightGoWrong()
{
    bool error1 = false;
    bool error2 = true;

    if (error1)
    {
        throw "Something went wrong!";
    }

    if (error2)
    {
        throw std::string("Something went wrong!");
    }

}

int main()
{
    try
    {
        mightGoWrong();
    }
    catch (int e)
    {
        std::cout << "Error: " << e << std::endl;
    }
    catch(char const* e)
    {
        std::cout << "Error: " << e << std::endl;
    }
    catch(std::string &e)
    {
        std::cout << "String error: " << e << std::endl;
    }

    std::cout << "Still working";
    return 0;
}