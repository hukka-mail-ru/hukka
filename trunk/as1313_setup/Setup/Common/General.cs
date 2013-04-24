// -----------------------------------------------------------------------
// <copyright file="General.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Diagnostics;
    using System.IO;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class General
    {
        public static string GetAppVersion()
        {
            FileVersionInfo fvi = FileVersionInfo.GetVersionInfo(
                     Path.Combine(Settings.SourceDir, Settings.MainExecutable));

            return fvi.FileVersion;
        }

        public static void SqlQuery(string query)
        {
            string outputFile = Directory.GetCurrentDirectory() + @"\output.txt";
            string cmd = "sqlcmd.exe";
            string args = " -b -S " + Settings.SQLServer +
            " -U " + Settings.SQLUser +
            " -P " + Settings.SQLPassword +
            " -Q \"" + query + "\"" +
            " -o " + outputFile;


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
    }
}
