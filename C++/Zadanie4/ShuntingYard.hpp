#include <iostream>
#include <map>
#include <queue>
#include <stack>
#include <string>

class ShuntingYard
{
    private:
    std::vector<std::string> toConvert;
    std::queue<char> input;
    std::stack<char> operators;
    std::queue<char> output;
    static std::map<char, int> operatorPriority;
    static std::map<char, bool> rightAssociative;
    void addToQueue(std::string&);

    bool isOperator(char) const;
    bool higherOrEqualPriority(char, char) const;
    bool firstCase(char) const;
    bool secondCase(char) const;
    void toONP(std::string&);

    public:
    ShuntingYard(std::vector<std::string>& toConv);
    void convertToONP();

};