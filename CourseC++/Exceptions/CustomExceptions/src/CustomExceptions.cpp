#include <iostream>
#include <exception>

class MyException : public std::exception
{
    public:
    virtual const char* what() const noexcept
    {
        return "Something bad happened!";
    }
};

class Test
{
    public:
    void goesWrong()
    {
        throw MyException();
    }
};

int main()
{
    Test test1;
    try
    {
        test1.goesWrong();
    }
    catch (MyException &e)
    {
        std::cout << "Error: " << e.what() << std::endl;
    }
    std::cout << "Still working\n";
    return 0;
}