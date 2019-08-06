 #include <thread>
 #include <string>
 #include <mutex>
 #include <iostream>
 #include <fstream>

std::mutex mu;


class LogFile
{
    std::mutex m_mutex;
    std::ofstream m_f;

    public:
    LogFile()
    {
        m_f.open("log.txt");
    }

    ~LogFile()
    {
        m_f.close();
    }


    void shared_print(std::string msg, int id)
    {
        std::lock_guard<std::mutex> guard(m_mutex);
        m_f << msg << id << std::endl;
    }
    

};

void func1(LogFile& log)
{
    for(int i = 0; i > -100; i--)
    {
        log.shared_print(std::string("From t1: "), i);
    }
}

int main()
{
    LogFile log;

    std::thread t1(func1, std::ref(log));

    for (int i = 0; i < 100; i++)
    {
        log.shared_print(std::string("From main: "), i);
    }

    t1.join();

    return 0;
}