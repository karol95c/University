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
    std::cout << "addUser\n" ;
    pqxx::nontransaction N(connect);
    std::string maxsql = "SELECT id FROM dataid WHERE id=" + js["member"].dump()+";";
    
    std::cout << maxsql << "\n";
    pqxx::result R( N.exec( maxsql ));
    auto id = R.begin();
    std::string memberStr = js["member"].dump();
    memberStr.erase(std::remove(memberStr.begin(), memberStr.end(), '"'), memberStr.end());
    N.commit();
    if (!id[0].is_null())
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
    for (auto js : jsonVec)
    {
        std::cout << js << std::endl;
        bool status = false;
        if (js["leader"] != nullptr)
        {   
            if (!initRun)
            {
                showErrorStatus();
                return false;
            }
            status = leader(js["leader"]);
        }
        else if (js["trolls"] != nullptr)   status = trolls(js);
        else if (js["protest"] != nullptr) status = support(js["protest"], false);
        else if (js["support"] != nullptr)  status = support(js["support"], true);
        else if (js["upvote"] != nullptr)   status = upvote(js["upvote"], true);
        else if (js["downvote"] != nullptr) status = upvote(js["downvote"], false);
        else if (js["votes"] != nullptr)    status = votes(js["votes"]);
        else if (js["actions"] != nullptr)  status = actions(js["actions"]);
        else if (js["projects"] != nullptr) status = projects(js["projects"]);
        else if (js["open"] != nullptr)
        {
            status = true;
            showSuccessStatus();
        }
        if (!status) showErrorStatus();
        status = false;
    }
    return true;
}

bool ProjectAPI::checkCorrectness(json& js, bool leader, bool add)
{
    std::cout << "checkCorrectness\n" ;
    pqxx::nontransaction N(connect);
    std::string pass = js["password"].dump();
    std::replace(pass.begin(), pass.end(), '"', '\'');
    std::string sql = "SELECT leader, last_active FROM politician WHERE member=" + js["member"].dump() + " AND pass=" + pass +";";
    std::cout << sql << "\n";
    pqxx::result R( N.exec( sql ));
    N.commit();
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
                pqxx::work W(connect);
            updateTimestamp(js, W);
            W.commit();
            std::cout << " !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11"<< std::endl;
            std::cout << row[0].as<std::string>() << std::endl;
            std::cout << leader << std::endl;
            if ( leader and row[0].as<std::string>() == "f")
            {
                 std::cout << "User don't have leader credentials." << "\n"; 
                 return false;
            }

            std::cout << "_____________________________________________\n";
            std::cout << js["timestamp"] << std::endl;
                        std::cout << "_____________________________________________\n";
            // if (js["timestamp"] - 31556926 > row[1].as<int>())
            // {
            //     std::cout << "User is frozen." << "\n";
            //     return false;
            // }
        }
    }
    return true;
}


bool ProjectAPI::leader(json& js)
{
    std::cout << "leader\n" ;
    try {
        pqxx::work W(connect);
        std::string insert;
        /* Create a transactional object. */

        std::cout << js["member"].dump() << "\n";
        insert = "INSERT INTO dataid VALUES("+ js["member"].dump() + ");";
        W.exec(insert);
        std::string pass = js["password"].dump();
        std::replace(pass.begin(), pass.end(), '"', '\'');
        insert = "INSERT INTO politician VALUES("+ js["member"].dump() + ", " + pass +
                ", true, TO_TIMESTAMP(" + js["timestamp"].dump() + "), default, default);";
        W.exec(insert);
        showSuccessStatus();

        /* Execute SQL query */
        W.commit();
        std::cout << "Table dataid insert successfully" << std::endl;
        return true;
        }catch (const std::exception &e) {
            std::cerr << e.what() << std::endl;
            return false;
        }
    return true;
}

void ProjectAPI::updateTimestamp(json& js, pqxx::work& W)
{
    std::cout << "updateTimestamp\n" ;
        try {
        std::string update = "UPDATE politician SET last_active  = TO_TIMESTAMP(" + js["timestamp"].dump() +
            ") WHERE member=" + js["member"].dump() +";";

        /* Create a transactional object. */
        
        /* Execute SQL query */
        W.exec(update);
        std::cout << "Table politician(timestamp) updated successfully" << std::endl;
   } catch (const std::exception &e) {
      std::cerr << e.what() << std::endl;
   }  
}

bool ProjectAPI::support(json& js, bool support)
{
    std::cout << "support\n" ;
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
        if (R2.begin() != R2.end())
        {
            return false;
        }
        sql = "SELECT project, authorityid FROM projects WHERE project=" + js["project"].dump();

        pqxx::result R3( N.exec( sql ));

        N.commit();
        pqxx::work W(connect);
        if (R3.begin() == R3.end())
        {
            if (!addToDataID(static_cast<int>(js["project"]), W)) return false;
            std::cout << js<< std::endl;
            if(addToDataID(static_cast<int>(js["authority"]), W))
            {
                std::string insertAuthority = "INSERT INTO authorities VALUES(" + js["authority"].dump() + ");";
                W.exec(insertAuthority);
            }
            
            std::string insertTemp = "INSERT INTO projects VALUES("+ js["project"].dump() +", " + js["authority"].dump() + ");";

            /* Create a transactional object. */
            
            /* Execute SQL query */

            W.exec(insertTemp);

        }
        std::string authorityStr;
        if  (js["authority"] == nullptr)
        {
            authorityStr = R3.begin()[1].as<std::string>();
        }
        else
        {
            authorityStr = js["authority"].dump();
        }
        
        if(!addToDataID(static_cast<int>(js["action"]), W)) return false;
        std::string insert;
        std::string update;
        if (support)
        {
            insert = "INSERT INTO actions VALUES("+ js["action"].dump() +", " + js["project"].dump() +", " +
                authorityStr + ", true, "+ js["member"].dump() +");";
        }
        else
        {
            insert = "INSERT INTO actions VALUES("+ js["action"].dump() +", " + js["project"].dump() +", " +
                authorityStr + ", false, "+ js["member"].dump() +");";
        }
    
        W.exec(insert);
        W.commit();
        std::cout << "Table dataid insert successfullyffff" << std::endl;
        showSuccessStatus();
        return true;
    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }

    showSuccessStatus();
    return true;

}

bool ProjectAPI::upvote(json& js, bool upvote)
{
    std::cout << "upvote\n" ;
    try {
        if(!checkCorrectness(js, false, true))
        {
            return false;
        }

        pqxx::nontransaction N(connect);
        std::string sql = "SELECT memberid FROM actions WHERE action=" + js["action"].dump()+";";
        std::cout << sql << std::endl;
        pqxx::result R1( N.exec( sql ));
        if (R1.begin() == R1.end())
        {
            return false;
        }
        sql = "SELECT actionid FROM votes WHERE memberid=" + js["member"].dump() + " AND actionid=" + js["action"].dump() +";";
        std::cout << sql << std::endl;
        pqxx::result R( N.exec( sql ));
        if (R.begin() != R.end())
        {
            return false;
        }
        N.commit();
        pqxx::work W(connect);
        std::string insert;
        std::string update;
        auto memberid = R1.begin();
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
        showSuccessStatus();
        return true;

    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }

    showSuccessStatus();
    return true;

}

bool ProjectAPI::actions(json& js)
{
    std::cout << "actions\n" ;
    try {
        bool first = false;
        if(!checkCorrectness(js, false, false))
        {
            return false;
        }

        pqxx::nontransaction N(connect);
        std::string sql = "SELECT action, support, projectid, authorityid, \
            SUM (CASE WHEN upvote=true THEN 1 ELSE 0 END) AS \"upvoteCount\", \
            SUM (CASE WHEN upvote=false THEN 1 ELSE 0 END) as \"downvoteCount\" \
            FROM ACTIONS \
            JOIN votes (action=votes.actionid)";
        if(js["support"] != nullptr)
        {
            first = true;
            sql += " WHERE support=true";
        }
        else if(js["protest"] != nullptr)
        {
            first = true;
            sql += " WHERE support=false";
        }

        if(js["project"] != nullptr)
        {
            if (first)
            {
                sql += " AND projectid=" + js["project"].dump();
            }
            else 
            {
                sql += " WHERE projectid=" + js["project"].dump();
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
                sql += " WHERE authorityid=" + js["authority"].dump();
            }
        }
        sql+=";";


        pqxx::result R1( N.exec( sql ));

        std::vector<json> returnVecJson;
        for (pqxx::result::const_iterator c = R1.begin(); c != R1.end(); ++c)
        {
            returnVecJson.push_back({c[0].as<int>(), c[1].as<std::string>(), c[2].as<int>(), c[3].as<int>(), c[4].as<int>(), c[5].as<int>()});
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
            returnVecJson.push_back({c[0].as<int>(), c[1].as<int>()});
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
            SUM (CASE WHEN upvote=true THEN 1 ELSE 0 END) as \"upvoteCount\", \
            SUM (CASE WHEN upvote=false THEN 1 ELSE 0 END) as \"downvoteCount\" \
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

        upvotesSql +=" GROUP BY member ORDER BY member;";
        pqxx::result R( N.exec( upvotesSql ));
 
        for (pqxx::result::const_iterator c = R.begin(); c != R.end(); ++c)
        {
            returnVecJson.push_back({c[0].as<int>(), c[1].as<int>(), c[2].as<int>()});
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
        std::string sql = "SELECT member, last_active, downvotes, upvotes FROM politician WHERE upvotes>downvotes";

        pqxx::result R( N.exec( sql ));
 
        for (pqxx::result::const_iterator c = R.begin(); c != R.end(); ++c)
        {
            if (static_cast<int>(js["timestamp"]) - 31556926 > c[1].as<int>())
            {
                returnVecJson.push_back({c[0].as<int>(), c[2].as<int>(), c[3].as<int>(), "false"});
            }
            else
            {
                returnVecJson.push_back({c[0].as<int>(), c[2].as<int>(), c[3].as<int>(), "true"});
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


bool ProjectAPI::addToDataID(int id, pqxx::work& W)
{
std::cout << "addToDataId\n" ;
    try {
        std::string insert = "INSERT INTO dataid VALUES("+ std::to_string(id) + ");";
        W.exec(insert);

        std::cout << "Table dataid insert successfully" << std::endl;
        return true;

    } catch (const std::exception &e) {
        std::cerr << e.what() << std::endl;
        return false;
    }
    return true;
}

void ProjectAPI::showSuccessStatus()
{
    std::cout << "showSuccessStatus\n" ;
    json j;
    j["status"] = "OK";
    std::cout << j << "\n";
}

void ProjectAPI::showErrorStatus()
{   
    std::cout << "showErrorStatus\n" ;
    json j;
    j["status"] = "ERROR";
    std::cout << j << "\n";
}