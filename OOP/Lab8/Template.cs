using System;
using System.Xml.Linq;
using System.IO;
using System.Text;
using System.Xml;
using System.Data.SqlClient;

namespace Template
{

    public class DataBaseHandler : DataAccessHandler
    {
        string connectionStr;
        string column;
        string columnFrom;
        SqlCommand cmd;
        SqlDataReader dreader;
  
        // for the connection to  
        // sql server database 
        SqlConnection connection;
        public DataBaseHandler(string server, string name, string userID, string password, string columnFrom, string column)
        {
            this.connectionStr = String.Format(@"Data Source={0};Initial Catalog={1};User ID={2};Password={3}",
                server, name, userID, password);
            this.column = column;
            this.columnFrom = columnFrom;
         }   

        public override void Connect(){
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
        public override void Download(){
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
        public override void Handle(){
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
        public override void Close(){
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

    public class XMLBaseHandler : DataAccessHandler
    {
        string fileName;
        string element;
        XmlTextReader xmlReader;
        public XMLBaseHandler(string fileName)
        {
            this.fileName = fileName;
            this.element = "";
        }
        public override void Connect(){
            xmlReader = new XmlTextReader(fileName);
        }
        public override void Download(){
        }
        public override void Handle(){
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
        public override void Close(){
            Console.WriteLine(element);
        }
    }
    public abstract class DataAccessHandler
    {
        public abstract void Connect();
        public abstract void Download();
        public abstract void Handle();
        public abstract void Close();


        public void Execute()
        {
            this.Connect();
            this.Download();
            this.Handle();
            this.Close();
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            DataAccessHandler xml = new XMLBaseHandler("books.xml");
            xml.Execute();

            DataAccessHandler db = new DataBaseHandler("server",
                    "name", "userID", "password", "columnFrom", "column");
            db.Execute();
        }
    }
}
