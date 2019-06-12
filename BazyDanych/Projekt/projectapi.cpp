#include "projectapi.h"

ProjectAPI::ProjectAPI(std::vector<json>& vec, pqxx::connection& C, int idx)
    : jsonVec(vec),
      connect(C)
{
    initRun = idx ? true : false;
    
}

ProjectAPI::~ProjectAPI()
{
}


bool ProjectAPI::addUser(json& js)
{
    pqxx::nontransaction N(connect);
    std::string maxsql = "SELECT MAX(id) FROM dataid;";
    
    std::cout << maxsql << "\n";
    pqxx::result R( N.exec( maxsql ));
    auto id = R.begin();
    std::string memberStr = js["member"].dump();
    memberStr.erase(std::remove(memberStr.begin(), memberStr.end(), '"'), memberStr.end());
    if (id[0].as<int>() > std::stoi(memberStr))
    {
        //TODO ERROR;
        showErrorStatus();
        return false;
    }
    else
    {
        try {
            pqxx::work W(connect);
            std::cout << js["member"].dump() << "\n";
            std::string insert = "INSERT INTO dataid VALUES("+ js["member"].dump() + ");";
            W.exec(insert);
            std::string pass = js["password"].dump();
            std::replace(pass.begin(), pass.end(), '"', '\'');
            insert = "INSERT INTO politician VALUES("+ js["member"].dump() + ", " + pass +
                    ", false, TO_TIMESTAMP(" + js["timestamp"].dump() + "), default, default);";
            W.exec(insert);
            W.commit();
            showSuccessStatus();
            } catch (const std::exception &e) {
                std::cerr << e.what() << std::endl;
                return false;
            } 
    }
    
}

bool ProjectAPI::process()
{
    std::cout << "process()\n";
    if (initRun)
    {
        try {
            pqxx::work W(connect);
            std::string insert;
            /* Create a transactional object. */
            for (auto js : jsonVec)
            {
                if (js["leader"] != nullptr)
                {
                    json js2 = js["leader"];
                    std::cout << js2["member"].dump() << "\n";
                    insert = "INSERT INTO dataid VALUES("+ js2["member"].dump() + ");";
                    W.exec(insert);
                    std::string pass = js2["password"].dump();
                    std::replace(pass.begin(), pass.end(), '"', '\'');
                    insert = "INSERT INTO politician VALUES("+ js2["member"].dump() + ", " + pass +
                                ", true, TO_TIMESTAMP(" + js2["timestamp"].dump() + "), default, default);";
                    W.exec(insert);
                    showSuccessStatus();
                }
            }

            /* Execute SQL query */
            W.commit();
            std::cout << "Table dataid insert successfully" << std::endl;
        } catch (const std::exception &e) {
            std::cerr << e.what() << std::endl;
            return false;
        } 

    }
    for (auto js : jsonVec)
    {
        if (js["leader"] != nullptr)
        {   
            if (!initRun)
            {
                std::cout << "You don't have credentials to use this function.\n";
                showErrorStatus();
                return false;
            }
        }
        bool status = false;
        if (js["protest"] != nullptr) status = support(js["protest"], false);
        else if (js["support"] != nullptr)  status = support(js["support"], true);
        else if (js["upvote"] != nullptr)   status = upvote(js, true);
        else if (js["downvote"] != nullptr) status = upvote(js, false);
        // else if (js["votes"] != nullptr)    status = votes(js);
        // else if (js["trolls"] != nullptr)   status = trolls(js);
        // else if (js["actions"] != nullptr)  status = actions(js);
        // else if (js["projects"] != nullptr) status = projects(js);
        status ? showSuccessStatus() : showErrorStatus();
    }
    return true;
}

bool ProjectAPI::checkCorrectness(json& js, bool leader, bool add)
{
    pqxx::nontransaction N(connect);
    std::string sql = "SELECT leader, last_active FROM politician WHERE member=" + js["member"].dump() + " AND pass=" + js["password"].dump() +";";
    std::cout << sql << "\n";
    pqxx::result R( N.exec( sql ));
    if (R.begin() == R.end())
    {   
        if(add)
        {
            addUser(js);
            return true;
        }
        return false;
    }
    else
    {
        for (auto row = R.begin(); row != R.end(); row++)
        {
            updateTimestamp(js);
            if (!(row[0].c_str() == (leader ? "true" : "false")))
            {
                 std::cout << "User don't have leader credentials." << "\n"; 
                 return false;
            }
            if (static_cast<int>(js["timestamp"]) - 31556926 > row[1].as<int>())
            {
                std::cout << "User is frozen." << "\n";
                return false;
            }
        }
    }
    return true;
}


void ProjectAPI::leader(json& js)
{

//     try {
//         pqxx::work W(connect);
//         std::string insert = "INSERT INTO dataid VALUES("+ js["member"].dump() + ");";

//         /* Create a transactional object. */
        
//         /* Execute SQL query */
//         W.exec(insert);
//         W.commit();
//         std::cout << "Table dataid insert successfully" << std::endl;
//    } catch (const std::exception &e) {
//       std::cerr << e.what() << std::endl;
//    }  

}

void ProjectAPI::updateTimestamp(json& js)
{
        try {
        pqxx::work W(connect);
        std::string update = "UPDATE politician SET last_active  = TO_TIMESTAMP(" + js["timestamp"].dump() +
            ") WHERE member=" + js["member"].dump() +";";

        /* Create a transactional object. */
        
        /* Execute SQL query */
        W.exec(update);
        W.commit();
        std::cout << "Table politician(timestamp) updated successfully" << std::endl;
   } catch (const std::exception &e) {
      std::cerr << e.what() << std::endl;
   }  
}

bool ProjectAPI::support(json& js, bool support)
{

    try {
        if(!checkCorrectness(js, false, true))
        {
            return false;
        }

        pqxx::nontransaction N(connect);
        std::string sql = "SELECT id FROM dataid WHERE id=" + js["action"].dump() +";";
        std::cout << sql << "\n";
        pqxx::result R( N.exec( sql ));
        if (R.begin() != R.end())
        {
            return false;
        }
        
        sql = "SELECT action FROM actions WHERE action=" + js["action"].dump();
        pqxx::result R2( N.exec( sql ));
        if (R2.begin() == R2.end())
        {
            return false;
        }

        sql = "SELECT project FROM projects WHERE project=" + js["project"].dump();
        pqxx::result R3( N.exec( sql ));
        pqxx::work W(connect);
        if (R3.begin() == R3.end())
        {
            if(!addToDataID(static_cast<int>(js["project"]))) return false;
            std::string insertTemp = "INSERT INTO project VALUES("+ js["project"].dump() +", " + js["authority"].dump() + ");";

            /* Create a transactional object. */
            
            /* Execute SQL query */
            W.exec(insertTemp);

        }
        if(!addToDataID(static_cast<int>(js["action"]))) return false;
        std::string insert;
        std::string update;
        if (support)
        {
            insert = "INSERT INTO action VALUES("+ js["action"].dump() +", " + js["project"].dump() +", " +
                js["authority"].dump() + ", true, "+ js["member"].dump() +");";
        }
        else
        {
            insert = "INSERT INTO action VALUES("+ js["action"].dump() +", " + js["project"].dump() +", " +
                js["authority"].dump() + ", false, "+ js["member"].dump() +");";
        }
    
        W.exec(insert);
        W.commit();
        std::cout << "Table dataid insert successfully" << std::endl;
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }  
    return true;

}

bool ProjectAPI::upvote(json& js, bool upvote)
{

    try {
        if(!checkCorrectness(js, false, true))
        {
            return false;
        }

        pqxx::nontransaction N(connect);
        std::string sql = "SELECT memberid FROM actions WHERE action=" + js["action"].dump()+";";
        pqxx::result R1( N.exec( sql ));
        if (R1.begin() != R1.end())
        {
            return false;
        }
        sql = "SELECT actionid FROM votes WHERE memberid" + js["member"].dump() + " AND actionid=" + js["action"].dump() +";";
        pqxx::result R( N.exec( sql ));
        if (R.begin() != R.end())
        {
            return false;
        }

        pqxx::work W(connect);
        std::string insert;
        std::string update;
        auto memberid = R.begin();
        if (upvote)
        {
            insert = "INSERT INTO votes VALUES("+ js["action"].dump() +", " + js["member"].dump() +", true);";
            update = "UPDATE politician SET upvotes=upvotes+1 WHERE member=" + memberid[0].as<std::string>() +";";
        }
        else
        {
            insert = "INSERT INTO votes VALUES("+ js["action"].dump() +", " + js["member"].dump() +", false);";
            update = "UPDATE politician SET downvotes=downvotes+1 WHERE member=" + memberid[0].as<std::string>() +";";
        }
    
        W.exec(insert);
        W.exec(update);
        W.commit();
        std::cout << "Table dataid insert successfully" << std::endl;

    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }  
    return true;

}

bool ProjectAPI::actions(json& js)
{
    try {
        bool first = false;
        if(!checkCorrectness(js, false, false))
        {
            return false;
        }

        pqxx::nontransaction N(connect);
        std::string sql = "SELECT action, authorityid, support FROM actions WHERE ";
        if(js["support"] != nullptr)
        {
            first = true;
            sql += "support=true";
        }
        else if(js["protest"] != nullptr)
        {
            first = true;
            sql += "support=false";
        }
        if(js["project"] != nullptr)
        {
            if (first)
            {
                sql += " AND projectid=" + js["project"].dump();
            }
            else 
            {
                sql += "projectid=" + js["project"].dump();
            }
        }
        else if(js["authority"] != nullptr)
        {
            if (first)
            {
                sql += " AND authorityid=" + js["authority"].dump();
            }
            else 
            {
                sql += "authorityid=" + js["authority"].dump();
            }
        }
        sql+=";";


        pqxx::result R1( N.exec( sql ));
        if (R1.begin() != R1.end())
        {
            return false;
        }

        std::vector<json> returnVecJson;
        std::string countTrue;
        std::string countFalse;
        for (pqxx::result::const_iterator c = R1.begin(); c != R1.end(); ++c)
        {
            countTrue = "SELECT COUNT(distinct memberid) FROM votes WHERE actionid=" + c[0].as<std::string>() + " AND upvote=true;";
            pqxx::result falseVotes( N.exec( countTrue));
            countFalse = "SELECT COUNT(distinct memberid) FROM votes WHERE actionid=" + c[0].as<std::string>() + " AND upvote=false;";
            pqxx::result trueVotes( N.exec( countFalse ));
            //std::vector<json> vec = {c[0], c[1], c[2], R.begin()[0]};
            json j({c[0], c[1], c[2], trueVotes.begin()[0], falseVotes.begin()[0]});
            returnVecJson.push_back(j);
        }
        json returnJson;
        returnJson["status"] = "OK";
        returnJson["data"] = returnVecJson;
        std::cout << returnJson << "\n";
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }  
    return true;
}

bool ProjectAPI::projects(json& js)
{
    try {
        if(!checkCorrectness(js, true, false))
        {
            return false;
        }

        pqxx::nontransaction N(connect);
        std::string sql = "SELECT project, authorityid FROM projects";
        if(js["authority"] != nullptr)
        {

            sql += " WHERE authorityid=" + js["authority"].dump();
        }

        sql+=";";
        pqxx::result R( N.exec( sql ));

        std::vector<json> returnVecJson;
        for (pqxx::result::const_iterator c = R.begin(); c != R.end(); ++c)
        {
            returnVecJson.push_back({c[0], c[1]});
        }

        json returnJson;
        returnJson["status"] = "OK";
        returnJson["data"] = returnVecJson;
        std::cout << returnJson << "\n";
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }  
    return true;
}

bool ProjectAPI::votes(json& js)
{
    try {
        if(!checkCorrectness(js, true, false))
        {
            return false;
        }
        bool first = false;
        pqxx::nontransaction N(connect);
        std::vector<json> returnVecJson;
        std::string upvotesSql = "SELECT member, \
            SUM(CASE WHEN upvote=true THEN 1 ELSE 0 END) as upvoteCount \
            SUM(CASE WHEN upvote=false THEN 1 ELSE 0 END) as downvoteCount\
            FROM politician \
            LEFT JOIN votes ON(memberid=member)";

        if(js["project"] != nullptr)
        {
            upvotesSql+= " LEFT JOIN projects ON(projects.actionid=votes.actionid) \
                WHERE projects.project="+ js["project"].dump();
             
        }
        else if(js["action"] != nullptr)
        {
            upvotesSql += " WHERE actionid=" + js["action"].dump();
        }

        upvotesSql +=" GROUP BY member, upvoteCount, dwonvoteCount ORDER BY member;";
        pqxx::result R( N.exec( upvotesSql ));
 
        for (pqxx::result::const_iterator c = R.begin(); c != R.end(); ++c)
        {
            returnVecJson.push_back({c[0], c[1]});
        }

        json returnJson;
        returnJson["status"] = "OK";
        returnJson["data"] = returnVecJson;
        std::cout << returnJson << "\n";
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }  
    return true;
}

bool ProjectAPI::trolls(json& js)
{
    try {
        pqxx::nontransaction N(connect);
        std::vector<json> returnVecJson;
        std::string sql = "SELECT member, last_activity, downvotes, upvotes FROM politician WHERE upvotes>downvotes";

        pqxx::result R( N.exec( sql ));
 
        for (pqxx::result::const_iterator c = R.begin(); c != R.end(); ++c)
        {
            if (static_cast<int>(js["timestamp"]) - 31556926 > c[1].as<int>())
            {
                returnVecJson.push_back({c[0], c[2], c[3], "false"});
            }
            else
            {
                returnVecJson.push_back({c[0], c[2], c[3], "true"});
            }
        }
        json returnJson;
        returnJson["status"] = "OK";
        returnJson["data"] = returnVecJson;
        std::cout << returnJson << "\n";
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }  
    return true;
}


bool ProjectAPI::addToDataID(int id)
{

    try {
        pqxx::work W(connect);
        std::string insert = "INSERT INTO dataid VALUES("+ std::to_string(id) + ");";
        W.exec(insert);
        W.commit();
        std::cout << "Table dataid insert successfully" << std::endl;

    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }
    return true;
}

void ProjectAPI::showSuccessStatus()
{
    json j;
    j["status"] = "OK";
    std::cout << j << "\n";
}

void ProjectAPI::showErrorStatus()
{
    json j;
    j["status"] = "ERROR";
    std::cout << j << "\n";
}