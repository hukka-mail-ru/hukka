// -----------------------------------------------------------------------
// <copyright file="Settings.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class Settings
    {
        public static string CompanyName = @"T-Systems";
        public static string ProductName = @"AS1313";

        public static string SourceDir = @"Client";
        public static string ScriptsDir = @"Scripts";
        public static string SourceExecutable = @"TuevSued.AS1313.exe";

        public static string DestinationFolder = @"D:\TEMP";
        public static string LibsFolder = @"C:\Program Files\Libs";

        public static string SQLServer = @"localhost\SQLExpress";
        public static string SQLUser = @"sa";
        public static string SQLPassword = @"Pincet11";

        public static string NetServer = @"10.11.12.13";
        public static string NetPort = @"1234";
        public static string NetUser = @"default";
        public static string NetDomain = @"domain.com";
    }
}
