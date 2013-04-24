// -----------------------------------------------------------------------
// <copyright file="Install.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.Linq;
    using System.Text;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class Install
    {
 
        public static void CreateFolders()
        {
            // company dir
            string companyDir = Path.Combine(Settings.DestinationFolder, Settings.CompanyName);
            if (!Directory.Exists(companyDir))
            {
                Directory.CreateDirectory(companyDir);
            }

            // product dir
            string productDir = Path.Combine(companyDir, Settings.SolutionName);
            if (!Directory.Exists(productDir))
            {
                Directory.CreateDirectory(productDir);
            }

            // version dir
            Settings.VersionDir = Path.Combine(productDir, General.GetAppVersion());

            if (!Directory.Exists(Settings.VersionDir))
            {
                Directory.CreateDirectory(Settings.VersionDir);
            }
        }


        public static void CopyFiles()
        {
            CopyDirectory(Settings.SourceDir, Settings.VersionDir);
        }


        public static void RunSqlScript()
        {
            string scriptsDir = Path.Combine(Directory.GetCurrentDirectory(), Settings.SourceDir, Settings.ScriptsDir);
            string script = Path.Combine(scriptsDir, "databaseAs1313.sql");
            string outputFile = Path.Combine(Directory.GetCurrentDirectory(), "RunSQLScriptOutput.txt");
            string cmd = "sqlcmd.exe";

            string args = 
                    " -S " + Settings.SQLServer + 
                    " -i \"" + script + "\"" +
                    " -v DirParam=\"" + scriptsDir + "\\\"" +
                    " -U " + Settings.SQLUser +
                    " -P " + Settings.SQLPassword + 
                    " -o " + outputFile;
    
            Process ExternalProcess = new Process();
            ExternalProcess.StartInfo.FileName = cmd;
            ExternalProcess.StartInfo.Arguments = args;
            ExternalProcess.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
            ExternalProcess.Start();
            ExternalProcess.WaitForExit();        
        }


        public static void ReplaceConfig()
        {
            string mainConfig = Path.Combine(Settings.VersionDir, Settings.MainConfig);
            string logsDir = Path.Combine(Settings.VersionDir, Settings.LogsDir);

            FindReplace(mainConfig, "data source=localhost;", "data source=" + Settings.SQLServer + ";");
            FindReplace(mainConfig, @"d:\Argetp21", Settings.LibsFolder);
            FindReplace(mainConfig, @"D:\Argetp21", Settings.LibsFolder);
            FindReplace(mainConfig, @"c:\logs", logsDir);

            // NETWORK  
            string clientConfig = Path.Combine(Settings.VersionDir, Settings.ConfigDir, "clients.config");

            FindReplace(clientConfig, "localhost", Settings.NetServer);
            FindReplace(clientConfig, "9080", Settings.NetPort);
            FindReplace(clientConfig, "ozavorot", Settings.NetUser);
            FindReplace(clientConfig, "t-systems.ru", Settings.NetDomain);     
        }


        public static void Rollback()
        {

        }


        private static void FindReplace(string file, string find, string replace)
        {
            var fileContents = System.IO.File.ReadAllText(file);

            fileContents = fileContents.Replace(find, replace);

            System.IO.File.WriteAllText(file, fileContents);
        }

        private class Folders
        {
            public string Source { get; private set; }
            public string Target { get; private set; }

            public Folders(string source, string target)
            {
                Source = source;
                Target = target;
            }
        }

        private static void CopyDirectory(string source, string target)
        {
            var stack = new Stack<Folders>();
            stack.Push(new Folders(source, target));

            while (stack.Count > 0)
            {
                var folders = stack.Pop();
                Directory.CreateDirectory(folders.Target);
                foreach (var file in Directory.GetFiles(folders.Source, "*.*"))
                {
                    string targetFile = Path.Combine(folders.Target, Path.GetFileName(file));
                    if (File.Exists(targetFile)) File.Delete(targetFile);
                    File.Copy(file, targetFile);
                }

                foreach (var folder in Directory.GetDirectories(folders.Source))
                {
                    stack.Push(new Folders(folder, Path.Combine(folders.Target, Path.GetFileName(folder))));
                }
            }
        }
    }
}
