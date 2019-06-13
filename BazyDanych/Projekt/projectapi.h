#include <iostream>
#include <pqxx/pqxx>
#include <nlohmann/json.hpp>
#include <fstream>

using json = nlohmann::json;

class ProjectAPI
{
    private:
    std::vector<json>& jsonVec;
    pqxx::connection& connect;
    bool initRun;

    bool initialized;

    public:
    ProjectAPI(std::vector<json>& vec, pqxx::connection& C, int idx);
    ~ProjectAPI();
    ProjectAPI(bool init);
    bool process();
    private:
    void handleQuery();

    private:

    bool addToDataID(int id, pqxx::work& W);
    bool checkCorrectness(json& js, bool leader, bool add);
    void updateTimestamp(json& j, pqxx::work& W);
    void showSuccessStatus();
    void showErrorStatus();
    bool addUser(json& js);
    bool leader(json& js);
    bool support(json& js, bool support);
    bool upvote(json& js, bool upvote);
    bool votes(json& js);
    bool trolls(json& js);
    bool actions(json& js);
    bool projects(json& js);

};