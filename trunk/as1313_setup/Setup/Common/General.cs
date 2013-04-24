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


        public static void Rollback()
        {
            Files.Delete();
            WinRegistry.Unregiser();
            Database.Drop();
        }
   
    }
}
