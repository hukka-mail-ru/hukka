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
            string productDir = Path.Combine(companyDir, Settings.ProductName);
            if (!Directory.Exists(productDir))
            {
                Directory.CreateDirectory(productDir);
            }

            // version dir
            FileVersionInfo fvi = FileVersionInfo.GetVersionInfo(
                     Path.Combine(Settings.ClientDir, Settings.ClientExecutable));

            string versionDir = Path.Combine(productDir, fvi.FileVersion);

            if (!Directory.Exists(versionDir))
            {
                Directory.CreateDirectory(versionDir);
            }
        }




        public static void CopyFiles()
        {

        }


        public static void RunSqlScript()
        {

        }


        public static void ReplaceConfig()
        {

        }


        public static void Rollback()
        {

        }
    }
}
