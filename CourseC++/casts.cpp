#include <iostream>
#include <stdio.h>

class Parent
{
    public:
    virtual void speak()
    {
        std::cout << "Parent!" << std::endl;
    }
};


class Brother : public Parent
{
};

class Sister : public Parent
{
};

int main()
{
    Parent parent;
    Brother brother;
    Sister sister;

    Parent *ppb = &brother;

    Sister *pbb = dynamic_cast<Sister *>(ppb);

    if (pbb == nullptr)
    {
        std::cout << "Invalid cast!" << std::endl;
    }
    else
    {
        std::cout << pbb << std::endl;
    }

    if ( (fputs("Hello world", stdout)) == EOF) {
        fprintf(stderr, "Whoops, something went wrong");
    }
    return 0;
}