using System;
using System.Xml.Linq;
using System.IO;
using System.Text;
using System.Xml;
using System.Data.SqlClient;

namespace Template
{

    public class DataBaseHandlerStrategy : IDataAccessHandlerStrategy
    {
        string connectionStr;
        string column;
        string columnFrom;
        SqlCommand cmd;
        SqlDataReader dreader;
  
        // for the connection to  
        // sql server database 
        SqlConnection connection;
        public DataBaseHandlerStrategy (string server, string name, string userID, string password, string columnFrom, string column)
        {
            this.connectionStr = String.Format(@"Data Source={0};Initial Catalog={1};User ID={2};Password={3}",
                server, name, userID, password);
            this.column = column;
            this.columnFrom = columnFrom;
         }   

        public void Connect(){
            try
            {
                connection = new SqlConnection(connectionStr);
                if (connection != null)
                {
                    connection.Open();
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
        public void Download(){
            try
            {
                string sql = String.Format("Select {0} from {1};", column, columnFrom);
                cmd = new SqlCommand(sql, connection); 
                dreader = cmd.ExecuteReader();
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
        public void Handle(){
            Int64 sum = 0;
            try
            {
                while (dreader.Read()) { 
                    sum += dreader.GetInt64(0);
                }
                Console.WriteLine("Sum: " + sum);
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
        public void Close(){
            try
            {
                dreader.Close(); 
                cmd.Dispose(); 
                connection.Close();
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
    }

    public class XMLBaseHandlerStrategy : IDataAccessHandlerStrategy
    {
        string fileName;
        string element;
        XmlTextReader xmlReader;
        public XMLBaseHandlerStrategy(string fileName)
        {
            this.fileName = fileName;
            this.element = "";
        }
        public void Connect(){
            xmlReader = new XmlTextReader(fileName);
        }
        public void Download(){
        }
        public void Handle(){
            int max = 0;
            try
            {
                while (xmlReader.Read())
                {
                    switch (xmlReader.NodeType)
                    {
                        case XmlNodeType.Element:
                        if (xmlReader.Name.Length > max)
                        {
                            max = xmlReader.Name.Length;
                            this.element = xmlReader.Name;
                        }
                        break;
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
        public void Close(){
            Console.WriteLine(element);
        }
    }
    public class DataAccessHandler
    {
        IDataAccessHandlerStrategy strategy;
        public DataAccessHandler( IDataAccessHandlerStrategy strategy)
        {
            this.strategy = strategy;
        }

        public void Execute()
        {
            strategy.Connect();
            strategy.Download();
            strategy.Handle();
            strategy.Close();
        }
    }
    
    public interface IDataAccessHandlerStrategy
    {
        void Connect();
        void Download();
        void Handle();
        void Close();
    }
    class Program
    {
        static void Main(string[] args)
        {
            DataAccessHandler xml = new DataAccessHandler(new XMLBaseHandlerStrategy("books.xml"));
            xml.Execute();

            DataAccessHandler db = new DataAccessHandler(new DataBaseHandlerStrategy("server",
                    "name", "userID", "password", "columnFrom", "column"));
            db.Execute();
        }
    }
}
