// -----------------------------------------------------------------------
// <copyright file="Registry.cs" company="">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Setup.Common
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Reflection;
    using System.Text;
    using Microsoft.Win32;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class WinRegistry
    {
        private static string RegUninstallLocation = @"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall";


        public static bool IsAppInstalled()
        {
            RegistryKey key = null;
            using (RegistryKey parent = Registry.LocalMachine.OpenSubKey(RegUninstallLocation, true))
            {
                if (parent == null)
                {
                    throw new ExceptionInUninstaller("Uninstall registry key not found: " + RegUninstallLocation);
                }

                try
                {
                    key = parent.OpenSubKey(Settings.ProductName, true);
                    if (key != null)
                    {
                        Settings.VersionDir = (string)key.GetValue("VersionDir");
                    }
                }
                catch (Exception ex)
                {
                    Message.Show(ex);
                }
                finally
                {
                    if (key != null)
                    {
                        key.Close();
                    }
                }
            }

            return (key != null);
        }



        public static void Register()
        {

            using (RegistryKey parent = Registry.LocalMachine.OpenSubKey(RegUninstallLocation, true))
            {
                if (parent == null)
                {
                    throw new ExceptionInUninstaller("Uninstall registry key not found: " + RegUninstallLocation);
                }

                RegistryKey key = null;
                try
                {
                    key = parent.OpenSubKey(Settings.ProductName, true) ??
                            parent.CreateSubKey(Settings.ProductName);

                    if (key == null)
                    {
                        throw new ExceptionInUninstaller("Unable to create uninstaller");
                    }


                    //   Assembly asm = GetType().Assembly;
                    string version = General.GetAppVersion();
                    string exe = Path.Combine(Settings.VersionDir, Settings.MainExecutable);
                    //   string exe = "\"" + asm.CodeBase.Substring(8).Replace("/", "\\\\") + "\"";

                    key.SetValue("DisplayName", Settings.ProductName);
                    key.SetValue("ApplicationVersion", version);
                    key.SetValue("Publisher", Settings.CompanyName);
                    //  key.SetValue("DisplayIcon", exe);
                    key.SetValue("DisplayVersion", version);
                    // key.SetValue("URLInfoAbout", "http://www.blinemedical.com");
                    // key.SetValue("Contact", "support@mycompany.com");
                    key.SetValue("InstallDate", DateTime.Now.ToString("yyyyMMdd"));
                    key.SetValue("VersionDir", Settings.VersionDir);
                    key.SetValue("UninstallString", exe + " /uninstallprompt");
                }
                finally
                {
                    if (key != null)
                    {
                        key.Close();
                    }
                }

            }
        }


        public static void Unregiser()
        {
            using (RegistryKey parent = Registry.LocalMachine.OpenSubKey(RegUninstallLocation, true))
            {
                if (parent == null)
                {
                    throw new ExceptionInUninstaller("Uninstall registry key not found: " + RegUninstallLocation);
                }

                parent.DeleteSubKeyTree(Settings.ProductName, true);
            }
        }

    }
}
