#include "ShuntingYard.hpp"

#include <iostream>
#include <map>
#include <queue>
#include <stack>
#include <string>
#include <cctype>


ShuntingYard::ShuntingYard(std::vector<std::string>& toConv) : toConvert(toConv) {}

bool ShuntingYard::isOperator(char c) const
{
    return operatorPriority.find(c) != operatorPriority.end();
}

bool ShuntingYard::firstCase(char c) const
{
    return  not rightAssociative[operators.top()] && operatorPriority[operators.top()] >= operatorPriority[c];
}

bool ShuntingYard::secondCase(char c) const
{
    return rightAssociative[operators.top()] && operatorPriority[operators.top()] > operatorPriority[c];
}

void ShuntingYard::addToQueue(std::string& str)
{
    input = std::queue<char>();
    for (char c : str)
    {
        input.push(c);
    }
}

void ShuntingYard::toONP(std::string& expression)
{
    std::string result = "";
    addToQueue(expression);
    while (!input.empty())
    {
        char p = input.front();
        input.pop();
        if (p == ' ') continue;
        else if (isdigit(p) or isalpha(p))
        {
            output.push(p);
        }
        else if (isOperator(p))
        {
            if (!operators.empty())
            {
                while (isOperator(operators.top()) and (firstCase(p) or secondCase(p)))
                {
                    output.push(operators.top());
                    operators.pop();
                }
            }
            operators.push(p);

        }
        else if (p == '(')
        {
            operators.push(p);
        }
        else if (p == ')')
        {
            while (operators.top() != '(')
            {
                output.push(operators.top());
                operators.pop();
            }
            operators.pop();
        }


    }
    while (!operators.empty())
    {
        output.push(operators.top());
        operators.pop();
    }

    while  (!output.empty())
    {
        result += output.front();
        output.pop();
    }
    std::cout << result + "\n";
}

void ShuntingYard::convertToONP()
{
    for (std::string str : toConvert)
    {
        toONP(str);
    }
}

std::map<char, int> ShuntingYard::operatorPriority = {
    {'+', 1},
    {'-', 1},
    {'*', 2},
    {'/', 2},
    {'%', 2},
    {'^', 3}
};

std::map<char, bool> ShuntingYard::rightAssociative = {
    {'+', false},
    {'-', false},
    {'*', false},
    {'/', false},
    {'%', false},
    {'^', true}
};

int main()
{
    std::vector<std::string> testVector;
    testVector.push_back("(2+a)+10/20");
    testVector.push_back("3+4*2/(1-5)^2");
    testVector.push_back("12 + a * (b * c + d / e)");
    ShuntingYard *testPtr = new ShuntingYard(testVector);
    testPtr->convertToONP();
    delete testPtr;

    return 0;
}