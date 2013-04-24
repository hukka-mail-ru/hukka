// -----------------------------------------------------------------------
// <copyright file="Database.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.IO;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Diagnostics;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class Database
    {
        public static void SqlQuery(string query)
        {
            string outputFile = Directory.GetCurrentDirectory() + @"\output.txt";
            string cmd = "sqlcmd.exe";
            string args = " -b -S \"" + Settings.SQLServer + "\"" +
            " -U \"" + Settings.SQLUser + "\"" +
            " -P \"" + Settings.SQLPassword + "\"" +
            " -Q \"" + query + "\"" +
            " -o \"" + outputFile + "\"";

            Process ExternalProcess = new Process();
            ExternalProcess.StartInfo.FileName = cmd;
            ExternalProcess.StartInfo.Arguments = args;
            ExternalProcess.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
            ExternalProcess.Start();
            ExternalProcess.WaitForExit();

            string text = System.IO.File.ReadAllText(outputFile);

            if (text != "")
            {
                throw new ExceptionSqlError(text);
            }
        }

        public static void Create()
        {
            string scriptsDir = Path.Combine(Directory.GetCurrentDirectory(), Settings.SourceDir, Settings.ScriptsDir);
            string script = Path.Combine(scriptsDir, "databaseAs1313.sql");
            string outputFile = Path.Combine(Directory.GetCurrentDirectory(), "RunSQLScriptOutput.txt");
            string cmd = "sqlcmd.exe";

            string args =
                    " -S \"" + Settings.SQLServer + "\"" +
                    " -i \"" + script + "\"" +
                    " -v DirParam=\"" + scriptsDir + "\\\"" +
                    " -U \"" + Settings.SQLUser + "\"" +
                    " -P \"" + Settings.SQLPassword + "\"" +
                    " -o \"" + outputFile + "\"";

            Process ExternalProcess = new Process();
            ExternalProcess.StartInfo.FileName = cmd;
            ExternalProcess.StartInfo.Arguments = args;
            ExternalProcess.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
            ExternalProcess.Start();
            ExternalProcess.WaitForExit();
        }


        public static void Drop()
        {
            SqlQuery("DROP DATABASE TUEV_SUED");
        }


    }
}
