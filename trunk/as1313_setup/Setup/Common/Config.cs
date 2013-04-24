// -----------------------------------------------------------------------
// <copyright file="Config.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Text;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class Config
    {
        public static void ReplaceAll()
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


        private static void FindReplace(string file, string find, string replace)
        {
            var fileContents = System.IO.File.ReadAllText(file);

            fileContents = fileContents.Replace(find, replace);

            System.IO.File.WriteAllText(file, fileContents);
        }
    }
}
