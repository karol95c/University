#include <iostream>
#include <fstream>
#include <nlohmann/json.hpp>
#include "projectapi.h"
#include <string>
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


bool prepareDataBase(char* fileName, pqxx::connection& C)
{
    try
    {
        std::ifstream infile {fileName};
        std::string file_contents { std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>() };
        pqxx::work W(C);
        W.exec(file_contents);
        W.commit();
        return true;
    } catch (const std::exception &e) {
        std::cerr<< e.what() << std::endl;
        return false;
    } 
    return true;
}

bool isCommandLineMode(int argc, char *argv[])
{
    for (int i = 0; i < argc; ++i)
    {
      if (!strcmp(argv[i], "--cmd"))
      {
         return i;
      }
     }
   return 0;
}

int main(int argc, char *argv[])
{
    int initIdx = isInitRun(argc, argv);
    bool commandLine = isCommandLineMode(argc, argv);
    std::vector<json> jsonVec;
    std::string dbName;
    std::string userName;
    std::string userPassword;
    std::ifstream inputFile;
    std::string jsonLine;
    json first;

    if(commandLine)
    {
        std::getline(std::cin, jsonLine);
        if(jsonLine == "exit") return 0;
        jsonLine.erase(remove_if(jsonLine.begin(), jsonLine.end(), isspace),jsonLine.end());
        if (jsonLine.length() > 0 and jsonLine[0] != '-')
        {
            first = json::parse(jsonLine);
        }
    }
    else if (argc > 2 or (argc > 1 and !initIdx))
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
            if (inputLine.length() > 0 and inputLine[0] != '-')
            {
                jsonVec.push_back(std::move(json::parse(inputLine)));
            }
        }
        first = jsonVec[0];
        inputFile.close();
    }
    

    if (handleFirstJson(first, initIdx, dbName, userName, userPassword))
    {
        try {
            std::string connectStr = "dbname = " + dbName + " user = " + userName + " password = " + userPassword+ " \
                hostaddr = 127.0.0.1";
            pqxx::connection C(connectStr);

            if (C.is_open())
            {   
                if (initIdx)
                {
                    if (!prepareDataBase("database.psql", C))
                    {
                        std::cout << "Cannot create database from file." << std::endl;
                    }
                }

                ProjectAPI* papi = new ProjectAPI(jsonVec, C, initIdx);
                if(commandLine)
                {   
                    json processjs;
                    while(true)
                    {
                        std::getline(std::cin, jsonLine);
                        if(jsonLine == "exit") break;
                        jsonLine.erase(remove_if(jsonLine.begin(), jsonLine.end(), isspace), jsonLine.end());
                        
                        if (jsonLine.length() > 0)
                        {
                            processjs = json::parse(jsonLine);
                            papi->processJson(processjs);
                        }

                    }
                }
                else
                {
                    papi->process();
                }
                delete papi;

            }
            else
            {
                 std::cout << "Cannot open database." << std::endl;
            }

            C.disconnect ();
        } catch (const std::exception &e) {
            std::cerr << e.what() << std::endl;
        }  

    }

  return 0;
}