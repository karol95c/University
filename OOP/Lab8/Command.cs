using System;
using System.Collections.Generic;
using System.Net;
using System.Threading;
using System.IO;

namespace Command
{
    public interface ICommand
    {
        void Execute();
    }
    public class Invoker
    {
        private Queue<ICommand> commands;
        AutoResetEvent waitHandle = new AutoResetEvent(false);
        private static Semaphore semaphore;
        private object lockThread;
        Thread t1;
        Thread t2;
        bool running;
        public Invoker()
        {
            running = true;
            commands = new Queue<ICommand>();
            semaphore = new Semaphore(0, 2);
            lockThread = new object();
            t1 = new Thread (ExecuteCommand);         
            t1.Start();
            t2 = new Thread (ExecuteCommand);
            t2.Start();
        }

        public void Finish()
        {
            running = false;
            try {
                semaphore.Release();
            }
            catch (Exception e)  
            {  
                Console.WriteLine(e.Message);  
            } 
        }

        public void SetCommand(ICommand command)
        {
            lock (lockThread)
            {
                commands.Enqueue(command);
            }
            try {
                lock (lockThread)
                {
                    if (commands.Count == 1)
                    {
                        semaphore.Release();
                    }
                }
            }
            catch (Exception e)  
            {
                //can happen, could be ignore silently
                Console.WriteLine(e.Message);  
            } 
        }

        public void ExecuteCommand()
        {
            bool wait = false;
            while(running || commands.Count != 0)
            {
                lock (lockThread)
                {
                    if (commands.Count == 0)
                    {
                        wait = true;
                    }
                }
                if (wait)
                {
                    semaphore.WaitOne();
                }

                ICommand c = null;
                lock (lockThread)
                {
                    if (commands.Count != 0)
                    {
                        c = commands.Dequeue();
                    }
                }
                if (c!=null)
                {
                    c.Execute();
                }
                try {
                    lock (lockThread)
                    {
                        if (commands.Count != 0)
                        {
                            semaphore.Release();
                        }
                    }
                }
                catch (Exception e)  
                {  
                    Console.WriteLine(e.Message);  
                } 
            }
        }
    }

    public class HTTPCommand : ICommand
    {
        string url;
        string fileName;

        public HTTPCommand(string url, string fileName)
        {
            // NetworkCredential credential = new NetworkCredential(Properties.Settings.Default.FTPUserName, Properties.Settings.Default.FTPPassword);
            this.url = url;
            this.fileName = fileName;
        }

        public void Execute()
        {
            try {
                using (var client = new WebClient())
                {
                    client.DownloadFile(url, fileName);
                }
            }
            catch (Exception e)  
            {  
                Console.WriteLine(e.Message);  
            } 
        }
    }

    public class RandomCommand : ICommand
    {
        string fileName;

        public RandomCommand(string fileName)
        {
            this.fileName = fileName;
        }

        public void Execute()
        {
            const int sizeInMb = 1;
            const int blockSize = 1024 * 8;
            const int blocksPerMb = (1024 * 1024) / blockSize;
            byte[] data = new byte[blockSize];
            Random rng = new Random();
            try{
                using (FileStream stream = File.OpenWrite(fileName))
                {
                    // There 
                    for (int i = 0; i < sizeInMb * blocksPerMb; i++)
                    {
                        rng.NextBytes(data);
                        stream.Write(data, 0, data.Length);
                    }
                }               
            }
            catch (Exception e)  
            {  
                Console.WriteLine(e.Message);  
            } 
        }
    }

    public class FTPCommand : ICommand
    {
        string ftpUrl;
        string fileName;
        NetworkCredential credentials;
        public FTPCommand(string url, string fileName, NetworkCredential credentials)
        {
            this.ftpUrl = url;
            this.fileName = fileName;
            this.credentials = credentials;
        }

        public void Execute()
        {
            try{
                WebClient client = new WebClient();
                client.Credentials = credentials;
                client.DownloadFile(ftpUrl, fileName);
            }
            catch (Exception e)  
            {  
                Console.WriteLine(e.Message);  
            } 
        }
    }

    public class CopyCommand : ICommand
    {
        string sourceFile;
        string destinationFile;
        public CopyCommand(string s, string d)
        {
            sourceFile = s;
            destinationFile = d;
        }

        public void Execute()
        {
            try  
            {  
                File.Copy(sourceFile, destinationFile, true);  
            }  
            catch (IOException iox)  
            {  
                Console.WriteLine(iox.Message);  
            } 
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Invoker invoker = new Invoker();
            //Examples each for type
            NetworkCredential credentials = new NetworkCredential("username", "password");
            invoker.SetCommand(new HTTPCommand("https://google.com/allinternet.txt", "all_internet_on_my_pc.txt"));
            invoker.SetCommand(new FTPCommand("ftp://example_ftp_link/weird_things.txt", "weird_things_on_my_pc.txt", credentials));
            invoker.SetCommand(new CopyCommand("source.txt", "destination.txt"));
            invoker.SetCommand(new RandomCommand("new_file.txt"));
            invoker.Finish();
        }
    }
}
