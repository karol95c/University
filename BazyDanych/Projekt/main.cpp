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
    std::cout << firstJsonStr << std::endl;
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
        for (auto x: jsonVec)
        {
            std::cout << x << std::endl;
        }

    }
    inputFile.close();

    if (handleFirstJson(jsonVec[0], initIdx, dbName, userName, userPassword))
    {
        try {
            std::string connectStr = "dbname = " + dbName + " user = " + userName + " password = " + userPassword+ " \
                hostaddr = 127.0.0.1";
            std::cout << connectStr << std::endl;
            pqxx::connection C(connectStr);

            if (C.is_open())
            {
                std::cout << "Opened database successfully: " << C.dbname() << std::endl;
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

    std::vector<int> c_vector {1, 2, 3, 4};
    std::vector<int> a_vec {1, 2, 3, 4};
    std::vector<std::vector<int>> vec;
    vec.push_back(c_vector);
    vec.push_back(a_vec);
    json a;
    json j(vec);
    a["status"] = "OK";
    a["data"] = j;
    std::cout << j << std::endl;


   // else
   // {
   //    createAppUser();
   // }
//   try {
//       char* sql;
//       char* insert;
//       pqxx::connection C("dbname = projectdb user = init password = qwerty \
//       hostaddr = 127.0.0.1");
//       if (C.is_open()) {
//          std::cout << "Opened database successfully: " << C.dbname() << std::endl;
//       } else {
//          std::cout << "Can't open database" << std::endl;
//          return 1;
//       }
//       /* Create SQL statement */
//       sql = "SELECT * FROM dataid;";

//       /* Create a transactional object. */
//       pqxx::work W(C);
//       insert = "INSERT INTO dataid VALUES (100011);";

//       /* Create a transactional object. */
      
//       /* Execute SQL query */
//       W.exec(insert);
//       W.commit();
//       std::cout << "Table dataid insert successfully" << std::endl;
//       pqxx::nontransaction N(C);

//       /* Execute SQL query */
      
//       /* Execute SQL query */
//       pqxx::result R( N.exec( sql ));
      
//       /* List down all the records */
//       for (pqxx::result::const_iterator c = R.begin(); c != R.end(); ++c) {
//          std::cout << "ID = " << c[0].as<int>() << std::endl;
//       }
//       std::cout << "Table read successfully" << std::endl;

//       C.disconnect ();
//    } catch (const std::exception &e) {
//       std::cerr << e.what() << std::endl;
//       return 1;
//    }  
//   std::cout << "DZIALA?" << std::endl;
  return 0;
}