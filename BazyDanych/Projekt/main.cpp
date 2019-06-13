#include <iostream>
#include <fstream>
#include <nlohmann/json.hpp>
#include "projectapi.h"

// for convenience
using json = nlohmann::json;

int isInitRun(int argc, char *argv[])
{
   for (int i = 0; i < argc; ++i)
   {
      if (!strcmp(argv[i], "--init"))
      {
         return i;
      }
   }
   return 0;
}

bool checkCredentials(std::string login, std::string& userName, std::string& userPassword)
{
    if (userName != login or userPassword != "qwerty")
    {
        std::cout << "Wrong credentials." <<std::endl;
        return false;
    }
    return true;
}

bool handleFirstJson(json& firstJson, const int idx, std::string& dbName, std::string& userName, std::string& userPassword)
{
    std::string firstJsonStr = firstJson.dump();
    std::cout << firstJson["open"] << std::endl;
    if (firstJson["open"] != nullptr)
    {
        bool status = false;
        userName = firstJson["open"]["login"];
        userPassword = firstJson["open"]["password"];
        if(idx)
        {
            status = checkCredentials("init", userName, userPassword);
        }
        else
        {
            status = checkCredentials("app", userName, userPassword);
        }
        
        dbName = firstJson["open"]["database"];    
        return status;  
    }
}

bool checkIfInitSuccess()
{
    
}

int main(int argc, char *argv[])
{
    int initIdx = isInitRun(argc, argv);
    std::vector<json> jsonVec;
    std::string dbName;
    std::string userName;
    std::string userPassword;
    std::ifstream inputFile;
    if (argc > 2 or (argc > 1 and !initIdx))
    {
        std::string inputLine;
        char* fileName;
        if (initIdx == 1)
        {
            fileName = argv[2];
        }
        else
        {
            fileName = argv[1];
        }
        inputFile.open(fileName, std::ifstream::in);
        while (inputFile.good())
        {
            std::getline(inputFile, inputLine);
            jsonVec.push_back(std::move(json::parse(inputLine)));
        }

    }
    inputFile.close();

    if (handleFirstJson(jsonVec[0], initIdx, dbName, userName, userPassword))
    {
        try {
            std::string connectStr = "dbname = " + dbName + " user = " + userName + " password = " + userPassword+ " \
                hostaddr = 127.0.0.1";
            pqxx::connection C(connectStr);

            if (C.is_open())
            {   
                ProjectAPI* papi = new ProjectAPI(jsonVec, C, initIdx);
                papi->process();
                delete papi;

            }
            else
            {
                 std::cout << "Can't open database" << std::endl;
            }

            C.disconnect ();
        } catch (const std::exception &e) {
            std::cerr << e.what() << std::endl;
        }  

    }

  return 0;
}