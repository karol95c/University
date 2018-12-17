#include <iostream>
#include <random>
#include <ctime>
#include <fstream>
#include <functional>

// template constructor(T arg)
class RandomGenerator
{
    std::random_device rd;
    std::mt19937_64 generator;
    std::vector<std::function<double()>> funcVec;
    
    std::uniform_real_distribution<> uniformDist;
    std::binomial_distribution<> binomialDist;
    std::poisson_distribution<> poissonDist;
    std::normal_distribution<> normalDist;

    const std::vector<std::string> fileNames{
        "Uniform",
        "Binomial",
        "Poisson",
        "Normal"
    };

    public:
    RandomGenerator() : generator(std::mt19937_64(rd())),
        uniformDist(0.0, 1.0), binomialDist(1000, 0.5),
        poissonDist(100), normalDist(5, 2)
    {
        funcVec.reserve(4);
        funcVec.push_back(returnUniform);
        funcVec.push_back(returnBinomial);
        funcVec.push_back(returnPoisson);
        funcVec.push_back(returnNormal);

    }
    void generateAndWriteToFile();

    void saveToFile(std::function<double()>& distributionCall, const std::string& fn)
    {
        std::ofstream outputFile(fn + ".csv");
        for (int i = 0; i < 1000; ++i)
        {
            outputFile << distributionCall()<< "\n";
        }
        outputFile.close();
    }

    void generateAll()
    {
        for (int i = 0; i < funcVec.size(); ++i)
        {
            saveToFile(funcVec[i], fileNames[i]);
        }
    }

    std::function<double()> returnUniform = [&]()
    {
        return uniformDist(generator);
    };

    std::function<double()> returnBinomial = [&]()
    {
        return binomialDist(generator);
    };

    std::function<double()> returnPoisson = [&]()
    {
        return poissonDist(generator);
    };

    std::function<double()> returnNormal = [&]()
    {
        return normalDist(generator);
    };
};


int main()
{
    RandomGenerator RG;
    RG.generateAll();
    return 0;
}